/*
 * xml, Copyright (C) 2002-2015   ProcessOne
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
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

#include "counted_allocs.inc"

typedef struct attr_t {
  char *name;
  char *val;
  struct attr_t *next;
} attr_t;

typedef struct event_t {
  int type;
  char *data;
  attr_t *attrs;
  struct event_t *next;
} event_t;

typedef struct {
  ErlNifEnv *env;
  ErlNifEnv *send_env;
  ErlNifPid *pid;
  attr_t *xmlns_attrs;
  event_t *events;
  size_t start;
  size_t end;
  char *root;
  XML_Parser parser;
  size_t size;
  size_t max_size;
} state_t;

static XML_Memory_Handling_Suite ms = {
  .malloc_fcn = enif_alloc,
  .realloc_fcn = enif_realloc,
  .free_fcn = enif_free
};

static ErlNifResourceType *parser_state_t = NULL;

static attr_t *alloc_attr(attr_t *next_attr)
{
  attr_t *attr = enif_alloc(sizeof(attr_t));
  ASSERT(attr);
  attr->name = NULL;
  attr->val = NULL;
  attr->next = next_attr;
  return attr;
}

static void free_attr(attr_t *attr)
{
  if (attr) {
    if (attr->name) enif_free(attr->name);
    if (attr->val) enif_free(attr->val);
    enif_free(attr);
  }
}

static event_t *alloc_event(event_t *next_event)
{
  event_t *event = enif_alloc(sizeof(event_t));
  ASSERT(event);
  event->data = NULL;
  event->attrs = NULL;
  event->next = next_event;
  return event;
}

static void free_event(event_t *event)
{
  if (event) {
    if (event->data) enif_free(event->data);
    while (event->attrs) {
      attr_t *attr = event->attrs;
      event->attrs = attr->next;
      free_attr(attr);
    }
    enif_free(event);
  }
}

int encode_name(const char *name, char **buf)
{
  char *attr_start;
  char *prefix_start;

  if ((attr_start = strchr(name, '\n'))) {
    if ((prefix_start = strchr(attr_start+1, '\n'))) {
      size_t attr_size = prefix_start - attr_start;
      size_t prefix_size = strlen(prefix_start) - 1;
      size_t buf_size = attr_size + prefix_size + 1;
      attr_start[attr_size] = 0;
      *buf = enif_alloc(buf_size);
      ASSERT(*buf);
      strcpy(*buf, prefix_start+1);
      (*buf)[prefix_size] = ':';
      strcpy(*buf + prefix_size + 1, attr_start+1);
    } else {
      *buf = enif_alloc(strlen(attr_start));
      ASSERT(*buf);
      strcpy(*buf, attr_start+1);
    };
  } else {
    *buf = enif_alloc(strlen(name) + 1);
    ASSERT(*buf);
    strcpy(*buf, name);
  }

  return 1;
}

static ERL_NIF_TERM str2bin(ErlNifEnv *env, char *s)
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

static ERL_NIF_TERM attrs2list(ErlNifEnv *env, attr_t *attr)
{
  ERL_NIF_TERM el;
  ERL_NIF_TERM list = enif_make_list(env, 0);

  while (attr) {
    el = enif_make_tuple2(env, str2bin(env, attr->name), str2bin(env, attr->val));
    list = enif_make_list_cell(env, el, list);
    attr = attr->next;
  }

  return list;
}

static ERL_NIF_TERM process_events(ErlNifEnv *env, event_t **events, int is_root)
{
  event_t *event;
  ERL_NIF_TERM name, el, children, tail;
  ERL_NIF_TERM els = enif_make_list(env, 0);

  while (*events) {
    event = *events;
    switch (event->type) {
    case XML_END:
      *events = event->next;
      free_event(event);
      children = process_events(env, events, 0);
      event = *events;
      if (event) {
	name = str2bin(env, event->data);
	el = enif_make_tuple4(env, enif_make_atom(env, "xmlel"),
			      name, attrs2list(env, event->attrs), children);
	els = enif_make_list_cell(env, el, els);
      }
      break;
    case XML_CDATA:
      el = enif_make_tuple2(env, enif_make_atom(env, "xmlcdata"),
			    str2bin(env, event->data));
      els = enif_make_list_cell(env, el, els);
      break;
    case XML_START:
      return els;
    }
    if (event) {
      *events = event->next;
      free_event(event);
    }
  }

  if (is_root) {
    if (enif_get_list_cell(env, els, &el, &tail))
      return el;
    else
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
			      str2bin(env, "unexpected XML error"));
  } else
    return els;
}

static void send_event(state_t *state, ERL_NIF_TERM el)
{
  state->size = 0;
  enif_send(state->env, state->pid, state->send_env,
	    enif_make_tuple2(state->send_env,
			     enif_make_atom(state->send_env, "$gen_event"),
			     enif_make_copy(state->send_env, el)));
  enif_clear_env(state->send_env);
}

static void send_all_state_event(state_t *state, ERL_NIF_TERM el)
{
  state->size = 0;
  enif_send(state->env, state->pid, state->send_env,
	    enif_make_tuple2(state->send_env,
			     enif_make_atom(state->send_env, "$gen_all_state_event"),
			     enif_make_copy(state->send_env, el)));
  enif_clear_env(state->send_env);
}

void *erlXML_StartElementHandler(state_t *state,
				 const XML_Char *name,
				 const XML_Char **atts)
{
  size_t i = 0;
  attr_t *attr = state->xmlns_attrs;
  state->xmlns_attrs = NULL;

  while (atts[i]) {
    attr = alloc_attr(attr);
    ASSERT(attr);
    ASSERT(encode_name(atts[i], &attr->name));
    attr->val = enif_alloc(strlen(atts[i+1]) + 1);
    ASSERT(attr->val);
    strcpy(attr->val, atts[i+1]);
    i += 2;
  }

  event_t *event = alloc_event(state->events);
  ASSERT(event);
  event->type = XML_START;
  event->attrs = attr;
  ASSERT(encode_name(name, &event->data));

  if (state->pid && !state->root) {
    state->root = event->data;
    send_event(state,
	       enif_make_tuple3(state->env,
				enif_make_atom(state->env, "xmlstreamstart"),
				str2bin(state->env, event->data),
				attrs2list(state->env, event->attrs)));
    event->data = NULL;
    free_event(event);
  } else {
    state->events = event;
    state->start++;
  }

  return NULL;
}

void *erlXML_CharacterDataHandler(state_t *state, const XML_Char *s, int len)
{
  if (state->pid && !state->start) {
    char *cdata = enif_alloc(len + 1);
    ASSERT(cdata);
    memcpy(cdata, s, len);
    cdata[len] = 0;
    send_all_state_event(state,
			 enif_make_tuple2(state->env,
					  enif_make_atom(state->env, "xmlstreamcdata"),
					  str2bin(state->env, cdata)));
    enif_free(cdata);
    return NULL;
  }

  if (state->events) {
    event_t *event = state->events;
    if (event->type == XML_CDATA) {
      size_t size = strlen(event->data);
      event->data = enif_realloc(event->data, size + len + 1);
      ASSERT(event->data);
      memcpy(event->data + size, s, len);
      event->data[size + len] = 0;
      return NULL;
    }
  }

  event_t *event = alloc_event(state->events);
  ASSERT(event);
  event->type = XML_CDATA;
  event->data = enif_alloc(len + 1);
  ASSERT(event->data);
  memcpy(event->data, s, len);
  event->data[len] = 0;

  state->events = event;

  return NULL;
}

void *erlXML_EndElementHandler(state_t *state, const XML_Char *name)
{
  event_t *event = alloc_event(state->events);
  ASSERT(event);
  event->type = XML_END;

  if (state->pid && !state->start) {
    send_event(state,
	       enif_make_tuple2(state->env,
				enif_make_atom(state->env, "xmlstreamend"),
				str2bin(state->env, state->root)));
    free_event(event);
    return NULL;
  }

  state->events = event;
  state->end++;

  if (state->pid && state->start == state->end) {
    state->start = 0;
    state->end = 0;
    ERL_NIF_TERM el = process_events(state->env, &state->events, 1);
    send_event(state,
	       enif_make_tuple2(state->env,
				enif_make_atom(state->env, "xmlstreamelement"),
				el));
  }

  return NULL;
}

void *erlXML_StartNamespaceDeclHandler(state_t *state,
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
      return NULL;

  attr_t *attr = alloc_attr(state->xmlns_attrs);
  ASSERT(attr);

  if (prefix) {
    attr->name = enif_alloc(strlen(prefix) + 7);
    ASSERT(attr->name);
    memcpy(attr->name, "xmlns:", 6);
    strcpy(attr->name + 6, prefix);
  } else {
    attr->name = enif_alloc(6);
    ASSERT(attr->name);
    strcpy(attr->name, "xmlns");
  };

  attr->val = enif_alloc(strlen(uri) + 1);
  ASSERT(attr->val);
  strcpy(attr->val, uri);

  state->xmlns_attrs = attr;

  return NULL;
}

/*
 * Prevent entity expansion attacks (CVE-2013-1664) by refusing
 * to process any XML that contains a DTD.
 */
void *erlXML_StartDoctypeDeclHandler(state_t *state,
				     const XML_Char *doctypeName,
				     const XML_Char *doctypeSysid,
				     const XML_Char *doctypePubid,
				     int hasInternalSubset)
{
  XML_StopParser(state->parser, PARSING_NOT_RESUMABLE);
  return NULL;
}

/*
 * Prevent entity expansion attacks (CVE-2013-1664) by having an explicit
 * default handler. According to the documentation,
 *
 * "Setting the handler with this call has the side effect of turning off
 *  expansion of references to internally defined general entities. Instead
 *  these references are passed to the default handler."
 */
void *erlXML_DefaultHandler(state_t *state, const XML_Char *s, int len)
{
  return NULL;
}

static void destroy_parser_state(ErlNifEnv *env, void *data)
{
  state_t *state = (state_t *) data;
  if (state) {
    if (state->parser) XML_ParserFree(state->parser);
    if (state->pid) enif_free(state->pid);
    if (state->send_env) enif_free_env(state->send_env);
    if (state->root) enif_free(state->root);
    while (state->xmlns_attrs) {
      attr_t *attr = state->xmlns_attrs;
      state->xmlns_attrs = attr->next;
      free_attr(attr);
    }
    while (state->events) {
      event_t *event = state->events;
      state->events = event->next;
      free_event(event);
    }
    memset(state, 0, sizeof(state_t));
  }
}

static state_t *init_parser_state(ErlNifPid *pid)
{
  state_t *state = enif_alloc_resource(parser_state_t, sizeof(state_t));
  ASSERT(state);
  memset(state, 0, sizeof(state_t));
  if (pid) {
    state->send_env = enif_alloc_env();
    state->pid = enif_alloc(sizeof(ErlNifPid));
    ASSERT(state->send_env);
    ASSERT(state->pid);
    memcpy(state->pid, pid, sizeof(ErlNifPid));
  }
  state->parser = XML_ParserCreate_MM("UTF-8", &ms, "\n");
  ASSERT(state->parser);
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

static ERL_NIF_TERM parse_element_nif(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM el;
  ErlNifBinary bin;

  if (argc != 1)
    return enif_make_badarg(env);

  if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
    return enif_make_badarg(env);

  state_t *state = init_parser_state(NULL);
  if (!state)
    return enif_make_badarg(env);

  int res = XML_Parse(state->parser, (char *)bin.data, bin.size, 1);
  if (res)
    el = process_events(env, &state->events, 1);
  else
    el = enif_make_tuple2(env, enif_make_atom(env, "error"),
			  make_parse_error(env, state->parser));

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

  if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
    return enif_make_badarg(env);

  if (!state->parser || !state->pid || !state->send_env)
    return enif_make_badarg(env);

  state->size += bin.size;
  state->env = env;

  if (state->size >= state->max_size) {
    send_event(state,
	       enif_make_tuple2(env,
				enif_make_atom(env, "xmlstreamerror"),
				str2bin(env, "XML stanza is too big")));
  } else {
    int res = XML_Parse(state->parser, (char *)bin.data, bin.size, 0);
    if (!res)
      send_event(state,
		 enif_make_tuple2(env,
				  enif_make_atom(env, "xmlstreamerror"),
				  make_parse_error(env, state->parser)));
  }

  return enif_make_resource(env, state);
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

static ERL_NIF_TERM memory_counter_nif(ErlNifEnv* env, int argc,
                                       const ERL_NIF_TERM argv[])
{
  if (argc != 0)
    return enif_make_badarg(env);

  return enif_make_long(env, allocated_xml);
}

static ErlNifFunc nif_funcs[] =
  {
    {"new", 2, new_nif},
    {"parse", 2, parse_nif},
    {"parse_element", 1, parse_element_nif},
    {"close", 1, close_nif},
    {"change_callback_pid", 2, change_callback_pid_nif},
    {"memory_counter_nif", 0, memory_counter_nif}
  };

ERL_NIF_INIT(xml_stream, nif_funcs, load, NULL, NULL, NULL)
