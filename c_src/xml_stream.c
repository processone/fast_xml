/*
 * Copyright (C) 2002-2015 ProcessOne, SARL. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <erl_nif.h>
#include <string.h>
#include <stdio.h>
#include <expat.h>

#define XML_START 0
#define XML_END   1
#define XML_CDATA 2
#define PARSING_NOT_RESUMABLE XML_FALSE

#define ASSERT(x) if (!(x)) return 0
#define PARSER_ASSERT(X, E) do { if (!(X)) { state->error = (E); XML_StopParser(state->parser, PARSING_NOT_RESUMABLE); return; } } while(0)
#define PARSER_MEM_ASSERT(x) PARSER_ASSERT((x), "enomem")

typedef struct children_list_t {
  union {
    ERL_NIF_TERM term;
    ErlNifBinary cdata;
  };
  struct children_list_t *next;
  char is_cdata;
} children_list_t;

typedef struct attrs_list_t {
  ERL_NIF_TERM tuple;
  struct attrs_list_t *next;
} attrs_list_t;

typedef struct xmlel_stack_t {
  ERL_NIF_TERM name;
  ERL_NIF_TERM attrs;
  children_list_t *children;
  struct xmlel_stack_t *next;
} xmlel_stack_t;


typedef struct {
  ErlNifEnv *env;
  ErlNifEnv *send_env;
  ErlNifPid *pid;
  size_t depth;
  size_t size;
  size_t max_size;
  XML_Parser parser;
  xmlel_stack_t *elements_stack;
  attrs_list_t *xmlns_attrs;
  const char *error;
} state_t;

static XML_Memory_Handling_Suite ms = {
  .malloc_fcn = enif_alloc,
  .realloc_fcn = enif_realloc,
  .free_fcn = enif_free
};

static ErlNifResourceType *parser_state_t = NULL;

static int encode_name(const char *name, ErlNifBinary *buf)
{
  char *attr_start;
  char *prefix_start;

  if ((attr_start = strchr(name, '\n'))) {
    if ((prefix_start = strchr(attr_start+1, '\n'))) {
      size_t attr_size = prefix_start - attr_start;
      size_t prefix_size = strlen(prefix_start) - 1;
      size_t buf_size = attr_size + prefix_size;
      attr_start[attr_size] = 0;
      ASSERT(enif_alloc_binary(buf_size, buf));
      memcpy(buf->data, prefix_start+1, prefix_size);
      buf->data[prefix_size] = ':';
      memcpy(buf->data+prefix_size+1, attr_start+1, attr_size-1);
    } else {
      int len = strlen(attr_start) - 1;
      ASSERT(enif_alloc_binary(len, buf));
      memcpy(buf->data, attr_start + 1, len);
    };
  } else {
    int len = strlen(name);
    ASSERT(enif_alloc_binary(len, buf));
    memcpy(buf->data, name, len);
  }

  return 1;
}

static ERL_NIF_TERM str2bin(ErlNifEnv *env, const char *s)
{
  ErlNifBinary bin;
  if (enif_alloc_binary(strlen(s), &bin)) {
    memcpy(bin.data, s, bin.size);
    return enif_make_binary(env, &bin);
  } else {
    return enif_make_tuple2(env,
			    enif_make_atom(env, "error"),
			    enif_make_atom(env, "enomem"));
  }
}

static void send_event(state_t *state, ERL_NIF_TERM el)
{
  state->size = 0;
  enif_send(state->env, state->pid, state->send_env,
	    enif_make_tuple2(state->send_env,
			     enif_make_atom(state->send_env, "$gen_event"),
			     el));
  enif_clear_env(state->send_env);
}

static void send_all_state_event(state_t *state, ERL_NIF_TERM el)
{
  state->size = 0;
  enif_send(state->env, state->pid, state->send_env,
	    enif_make_tuple2(state->send_env,
			     enif_make_atom(state->send_env, "$gen_all_state_event"),
			     el));
  enif_clear_env(state->send_env);
}

void erlXML_StartElementHandler(state_t *state,
                                const XML_Char *name,
                                const XML_Char **atts)
{
  int i = 0;
  ErlNifEnv *env = state->send_env;
  ERL_NIF_TERM attrs_term = enif_make_list(env, 0);
  ErlNifBinary name_bin;

  if (state->error)
    return;

  state->depth++;

  while (atts[i]) {
    i += 2;
  }
  i -= 2;

  while (i >= 0) {
    ErlNifBinary attr_name;
    ERL_NIF_TERM val;
    unsigned char *val_str;

    PARSER_MEM_ASSERT(encode_name(atts[i], &attr_name));

    int val_len = strlen(atts[i+1]);
    PARSER_MEM_ASSERT(val_str = enif_make_new_binary(env, val_len, &val));
    memcpy(val_str, atts[i+1], val_len);

    ERL_NIF_TERM el = enif_make_tuple2(env, enif_make_binary(env, &attr_name), val);
    attrs_term = enif_make_list_cell(env, el, attrs_term);
    i -= 2;
  }

  while (state->xmlns_attrs) {
    attrs_list_t *c = state->xmlns_attrs;
    attrs_term = enif_make_list_cell(env, c->tuple, attrs_term);
    state->xmlns_attrs = c->next;
    enif_free(c);
  }

  PARSER_MEM_ASSERT(encode_name(name, &name_bin));

  if (state->pid && state->depth == 1) {
    send_event(state,
	       enif_make_tuple3(env,
				enif_make_atom(env, "xmlstreamstart"),
				enif_make_binary(env, &name_bin),
                                attrs_term));
  } else {
    xmlel_stack_t *xmlel = enif_alloc(sizeof(xmlel_stack_t));
    PARSER_MEM_ASSERT(xmlel);

    xmlel->next = state->elements_stack;
    xmlel->name = enif_make_binary(env, &name_bin);
    xmlel->attrs = attrs_term;
    xmlel->children = NULL;

    state->elements_stack = xmlel;
  }
}

void erlXML_CharacterDataHandler(state_t *state, const XML_Char *s, int len)
{
  ErlNifEnv *env = state->send_env;

  if (state->error)
    return;

  if (state->depth == 0)
    return;

  if (state->pid && state->depth == 1) {
    ErlNifBinary cdata;
    PARSER_MEM_ASSERT(enif_alloc_binary(len, &cdata));
    memcpy(cdata.data, s, len);
    send_all_state_event(state,
			 enif_make_tuple2(env,
					  enif_make_atom(env, "xmlstreamcdata"),
					  enif_make_binary(env, &cdata)));
    return;
  }

  children_list_t *children = state->elements_stack->children;

  if (children && children->is_cdata) {
    int old_size = children->cdata.size;
    PARSER_MEM_ASSERT(enif_realloc_binary(&children->cdata, old_size + len));
    memcpy(children->cdata.data+old_size, s, len);
  } else {
    children = enif_alloc(sizeof(children_list_t));
    PARSER_MEM_ASSERT(children);
    if (!enif_alloc_binary(len, &children->cdata)) {
      enif_free(children);
      PARSER_MEM_ASSERT(0);
    }
    children->is_cdata = 1;
    memcpy(children->cdata.data, s, len);
    children->next = state->elements_stack->children;
    //fprintf(stderr, "LIST HD %p -> %p\n", children, children->next);
    state->elements_stack->children = children;
  }

  return;
}

ERL_NIF_TERM
make_xmlel_children_list(ErlNifEnv *env, children_list_t *list) {
  ERL_NIF_TERM children_list = enif_make_list(env, 0);

  while (list) {
    if (list->is_cdata)
      children_list = enif_make_list_cell(env,
                                          enif_make_tuple2(env,
                                                           enif_make_atom(env, "xmlcdata"),
                                                           enif_make_binary(env, &list->cdata)),
                                          children_list);
    else
      children_list = enif_make_list_cell(env, list->term, children_list);

    children_list_t *old_head = list;
    list = list->next;

    enif_free(old_head);
  }

  return children_list;
}

void erlXML_EndElementHandler(state_t *state, const XML_Char *name)
{
  ErlNifEnv *env = state->send_env;

  if (state->error)
    return;

  state->depth--;

  if (state->pid && state->depth == 0) {
    ErlNifBinary name_bin;

    PARSER_MEM_ASSERT(encode_name(name, &name_bin));

    send_event(state,
	       enif_make_tuple2(env,
				enif_make_atom(env, "xmlstreamend"),
                                enif_make_binary(env, &name_bin)));
    return;
  }

  ERL_NIF_TERM xmlel_term;

  xmlel_term = enif_make_tuple4(env, enif_make_atom(env, "xmlel"),
                                state->elements_stack->name,
                                state->elements_stack->attrs,
                                make_xmlel_children_list(env, state->elements_stack->children));

  if (state->elements_stack->next) {
    children_list_t *el;
    xmlel_stack_t *cur_el = state->elements_stack;

    PARSER_MEM_ASSERT(el = enif_alloc(sizeof(children_list_t)));

    state->elements_stack = state->elements_stack->next;

    el->is_cdata = 0;
    el->term = xmlel_term;
    el->next = state->elements_stack->children;
    //fprintf(stderr, "LIST HD %p -> %p\n", el, el->next);
    state->elements_stack->children = el;
    enif_free(cur_el);
  } else if (state->pid) {
    enif_free(state->elements_stack);
    state->elements_stack = NULL;
    send_event(state,
	       enif_make_tuple2(state->send_env,
				enif_make_atom(state->send_env, "xmlstreamelement"),
				xmlel_term));
  }

  return;
}

void erlXML_StartNamespaceDeclHandler(state_t *state,
                                      const XML_Char *prefix,
                                      const XML_Char *uri)
{
  /* From the expat documentation:
     "For a default namespace declaration (xmlns='...'),
     the prefix will be null ...
     ... The URI will be null for the case where
     the default namespace is being unset."

     FIXME: I'm not quite sure what all that means */
  if (uri == NULL)
      return;

  if (state->error)
    return;

  ErlNifEnv *env = state->send_env;
  ERL_NIF_TERM name_term, uri_term;
  unsigned char *name_str, *uri_str;

  if (prefix) {
    int len = strlen(prefix);

    PARSER_MEM_ASSERT(name_str = enif_make_new_binary(env, len + 6, &name_term));
    memcpy(name_str, "xmlns:", 6);
    memcpy(name_str + 6, prefix, len);
  } else {
    PARSER_MEM_ASSERT(name_str = enif_make_new_binary(env, 5, &name_term));
    memcpy(name_str, "xmlns", 5);
  };

  int len = strlen(uri);
  uri_str = enif_make_new_binary(env, len, &uri_term);

  memcpy(uri_str, uri, len);

  attrs_list_t *c = enif_alloc(sizeof(attrs_list_t));
  PARSER_MEM_ASSERT(c);

  c->tuple = enif_make_tuple2(env, name_term, uri_term);
  c->next = state->xmlns_attrs;
  state->xmlns_attrs = c;

  return;
}

/*
 * Prevent entity expansion attacks (CVE-2013-1664) by refusing
 * to process any XML that contains a DTD.
 */
void erlXML_StartDoctypeDeclHandler(state_t *state,
                                    const XML_Char *doctypeName,
                                    const XML_Char *doctypeSysid,
                                    const XML_Char *doctypePubid,
                                    int hasInternalSubset)
{
  XML_StopParser(state->parser, PARSING_NOT_RESUMABLE);
  return;
}

/*
 * Prevent entity expansion attacks (CVE-2013-1664) by having an explicit
 * default handler. According to the documentation,
 *
 * "Setting the handler with this call has the side effect of turning off
 *  expansion of references to internally defined general entities. Instead
 *  these references are passed to the default handler."
 */
void erlXML_DefaultHandler(state_t *state, const XML_Char *s, int len)
{
  return;
}

static void free_parser_allocated_structs(state_t *state) {
  while (state->xmlns_attrs) {
    attrs_list_t *c = state->xmlns_attrs;
    state->xmlns_attrs = c->next;
    enif_free(c);
  }
  while (state->elements_stack) {
    xmlel_stack_t *c = state->elements_stack;
    while (c->children) {
      children_list_t *cc = c->children;
      if (cc->is_cdata)
        enif_release_binary(&cc->cdata);
      c->children = cc->next;
      enif_free(cc);
    }
    state->elements_stack = c->next;
    enif_free(c);
  }
}

static void destroy_parser_state(ErlNifEnv *env, void *data)
{
  state_t *state = (state_t *) data;
  if (state) {
    if (state->parser) XML_ParserFree(state->parser);
    if (state->pid) enif_free(state->pid);
    if (state->send_env) enif_free_env(state->send_env);

    free_parser_allocated_structs(state);

    memset(state, 0, sizeof(state_t));
  }
}

static void setup_parser(state_t *state)
{
  XML_SetUserData(state->parser, state);
  XML_SetStartElementHandler(state->parser,
			     (XML_StartElementHandler) erlXML_StartElementHandler);
  XML_SetEndElementHandler(state->parser,
			   (XML_EndElementHandler) erlXML_EndElementHandler);
  XML_SetCharacterDataHandler(state->parser,
			      (XML_CharacterDataHandler) erlXML_CharacterDataHandler);
  XML_SetStartNamespaceDeclHandler(state->parser,
				   (XML_StartNamespaceDeclHandler)
				   erlXML_StartNamespaceDeclHandler);
  XML_SetStartDoctypeDeclHandler(state->parser,
				 (XML_StartDoctypeDeclHandler)
				 erlXML_StartDoctypeDeclHandler);
  XML_SetReturnNSTriplet(state->parser, 1);
  XML_SetDefaultHandler(state->parser, (XML_DefaultHandler) erlXML_DefaultHandler);
}

static state_t *init_parser_state(ErlNifPid *pid)
{
  state_t *state = enif_alloc_resource(parser_state_t, sizeof(state_t));
  ASSERT(state);
  memset(state, 0, sizeof(state_t));
  if (pid) {
    state->send_env = enif_alloc_env();
    ASSERT(state->send_env);
    state->pid = enif_alloc(sizeof(ErlNifPid));
    ASSERT(state->pid);
    memcpy(state->pid, pid, sizeof(ErlNifPid));
  }
  state->parser = XML_ParserCreate_MM("UTF-8", &ms, "\n");
  setup_parser(state);
  return state;
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  parser_state_t = enif_open_resource_type(env, NULL, "parser_state_t",
					   destroy_parser_state,
					   flags, NULL);
  return 0;
}

static ERL_NIF_TERM make_parse_error(ErlNifEnv *env, XML_Parser parser)
{
  int errcode = XML_GetErrorCode(parser);
  char *errstring;

  if (errcode == XML_STATUS_SUSPENDED)
    errstring = "DTDs are not allowed";
  else
    errstring = (char *)XML_ErrorString(errcode);

  return enif_make_tuple2(env, enif_make_uint(env, errcode),
			  str2bin(env, errstring));
}

static ERL_NIF_TERM reset_nif(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
  state_t *state = NULL;

  if (argc != 1)
    return enif_make_badarg(env);

  if (!enif_get_resource(env, argv[0], parser_state_t, (void *) &state))
    return enif_make_badarg(env);

  ASSERT(XML_ParserReset(state->parser, "UTF-8"));
  setup_parser(state);

  free_parser_allocated_structs(state);

  enif_clear_env(state->send_env);

  state->size = 0;
  state->depth = 0;
  state->error = NULL;

  return argv[0];
}

static ERL_NIF_TERM parse_element_nif(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM el;
  ErlNifBinary bin;

  if (argc != 1)
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  state_t *state = init_parser_state(NULL);
  if (!state)
    return enif_make_badarg(env);

  xmlel_stack_t *xmlel = enif_alloc(sizeof(xmlel_stack_t));
  if (!xmlel)
    return enif_make_badarg(env);

  state->send_env = env;

  xmlel->next = state->elements_stack;
  xmlel->children = NULL;

  state->elements_stack = xmlel;

  int res = XML_Parse(state->parser, (char *)bin.data, bin.size, 1);
  if (res && state->elements_stack->children && !state->elements_stack->children->is_cdata)
    el = state->elements_stack->children->term;
  else if (state->error)
    el = enif_make_tuple2(env, enif_make_atom(env, "error"),
                          enif_make_atom(env, state->error));
  else
    el = enif_make_tuple2(env, enif_make_atom(env, "error"),
			  make_parse_error(env, state->parser));

  state->send_env = NULL;

  enif_release_resource(state);
  return el;
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
  state_t *state = NULL;
  ErlNifBinary bin;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_get_resource(env, argv[0], parser_state_t, (void *) &state))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &bin))
    return enif_make_badarg(env);

  if (!state->parser || !state->pid || !state->send_env)
    return enif_make_badarg(env);

  state->size += bin.size;
  state->env = env;

  if (state->size >= state->max_size) {
    send_event(state,
	       enif_make_tuple2(state->send_env,
				enif_make_atom(state->send_env, "xmlstreamerror"),
				str2bin(state->send_env, "XML stanza is too big")));
  } else {
    int res = XML_Parse(state->parser, (char *)bin.data, bin.size, 0);
    if (!res)
      send_event(state,
                 enif_make_tuple2(state->send_env,
                                  enif_make_atom(state->send_env, "xmlstreamerror"),
                                  state->error ?
                                  str2bin(state->send_env, state->error) :
                                  make_parse_error(state->send_env, state->parser)));
  }

  return argv[0];
}

static ERL_NIF_TERM change_callback_pid_nif(ErlNifEnv* env, int argc,
					    const ERL_NIF_TERM argv[])
{
  state_t *state = NULL;
  ErlNifPid pid;

  if (argc != 2)
    return enif_make_badarg(env);

  if (!enif_get_resource(env, argv[0], parser_state_t, (void *) &state))
    return enif_make_badarg(env);

  if (!state->parser || !state->pid || !state->send_env)
    return enif_make_badarg(env);

  if (!enif_get_local_pid(env, argv[1], &pid))
    return enif_make_badarg(env);

  memcpy(state->pid, &pid, sizeof(ErlNifPid));

  return enif_make_resource(env, state);
}

static ERL_NIF_TERM close_nif(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
  state_t *state = NULL;

  if (argc != 1)
    return enif_make_badarg(env);

  if (!enif_get_resource(env, argv[0], parser_state_t, (void *) &state))
    return enif_make_badarg(env);

  if (!state->parser || !state->pid)
    return enif_make_badarg(env);

  destroy_parser_state(env, state);

  return enif_make_atom(env, "true");
}

static ERL_NIF_TERM new_nif(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
  if (argc != 2)
    return enif_make_badarg(env);

  ErlNifPid pid;
  if (!enif_get_local_pid(env, argv[0], &pid))
    return enif_make_badarg(env);

  state_t *state = init_parser_state(&pid);
  if (!state)
    return enif_make_badarg(env);

  ERL_NIF_TERM result = enif_make_resource(env, state);
  enif_release_resource(state);

  ErlNifUInt64 max_size;
  if (enif_get_uint64(env, argv[1], &max_size))
    state->max_size = (size_t) max_size;
  else if (!enif_compare(argv[1], enif_make_atom(env, "infinity")))
    state->max_size = (size_t) - 1;
  else
    return enif_make_badarg(env);

  return result;
}

static ErlNifFunc nif_funcs[] =
  {
    {"new", 2, new_nif},
    {"parse", 2, parse_nif},
    {"parse_element", 1, parse_element_nif},
    {"reset", 1, reset_nif},
    {"close", 1, close_nif},
    {"change_callback_pid", 2, change_callback_pid_nif}
  };

ERL_NIF_INIT(xml_stream, nif_funcs, load, NULL, NULL, NULL)
