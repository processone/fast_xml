/*
 * Copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
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
  ErlNifBinary name;
  ErlNifBinary value;
  struct attrs_list_t *next;
} attrs_list_t;

typedef struct xmlel_stack_t {
  ERL_NIF_TERM name;
  ERL_NIF_TERM attrs;
  children_list_t *children;
  struct xmlel_stack_t *next;
  char *namespace_str;
  int redefined_top_prefix;
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
  attrs_list_t *top_xmlns_attrs;
  const char *error;
  char normalize_ns:1;
  char gen_server:1;
  char use_maps:1;
} state_t;

typedef enum xmlns_op {
  OP_ERROR = 0,
  OP_REMOVE_PREFIX,
  OP_REMOVE_XMLNS,
  OP_REPLACE_XMLNS,
  OP_NOP
} xmlns_op;

static XML_Memory_Handling_Suite ms = {
  .malloc_fcn = enif_alloc,
  .realloc_fcn = enif_realloc,
  .free_fcn = enif_free
};

static ErlNifResourceType *parser_state_t = NULL;

#define FAKE_BIN(STR) { sizeof(STR)-1, (unsigned char*)STR }

static attrs_list_t stream_stream_ns_attr = {
  FAKE_BIN("stream:stream"),
  FAKE_BIN("http://etherx.jabber.org/streams")
};

static int same_str_buf(const char *str, const char *buf, size_t buf_len)
{
  if (strlen(str) != buf_len)
    return 0;
  if (!buf_len)
    return 1;
  return memcmp(str, buf, buf_len) == 0;
}

static char *dup_buf(const char *buf, size_t buf_len)
{
  char *res = enif_alloc(buf_len+1);
  if (!res)
    return NULL;

  if (buf_len)
    memcpy(res, buf, buf_len);
  res[buf_len] = '\0';

  return res;
}

static int dup_to_bin(ErlNifBinary *bin, const char *buf, size_t buf_len)
{
  if (!enif_alloc_binary(buf_len, bin))
      return 0;

  memcpy(bin->data, buf, buf_len);

  return 1;
}

static ERL_NIF_TERM dup_to_term(ErlNifEnv *env, const char *buf, size_t buf_len)
{
  ERL_NIF_TERM term;

  unsigned char *str = enif_make_new_binary(env, buf_len, &term);
  memcpy(str, buf, buf_len);

  return term;
}

static int has_prefix_ns_from_list(attrs_list_t*list, const char *pfx, size_t pfx_len,
                                   const char *ns, size_t ns_len)
{
  while (pfx_len && list) {
    if ((pfx == NULL ||
         (list->name.size == pfx_len && memcmp(list->name.data, pfx, pfx_len) == 0)) &&
        (ns == NULL ||
         (list->value.size == ns_len && memcmp(list->value.data, ns, ns_len) == 0)))
    {
      return 1;
    }
    list = list->next;
  }
  return 0;
}

static int has_prefix_ns_from_top(state_t *state, const char *pfx, size_t pfx_len,
                                  const char *ns, size_t ns_len)
{
  if (state->elements_stack->redefined_top_prefix || !pfx_len)
    return 0;

  return has_prefix_ns_from_list(state->top_xmlns_attrs, pfx, pfx_len, ns, ns_len);
}

static xmlns_op encode_name(state_t *state, const char *xml_name, ErlNifBinary *buf,
                            char **ns_str, char **pfx_str, int top_element)
{
  const char *parts[3];
  int i, idx = 0;

  for (i = 0; ; i++) {
    if (!xml_name[i] || xml_name[i] == '\n') {
      parts[idx++] = xml_name + i;
      if (!xml_name[i])
        break;
    }
    if (idx >= 3)
      return OP_ERROR;
  }
  const char *ns = NULL, *name = NULL, *prefix = NULL;
  size_t ns_len = 0, name_len = 0, prefix_len = 0;

  if (idx == 1) {
    name = xml_name;
    name_len = parts[0] - xml_name;
  } else {
    ns = xml_name;
    ns_len = parts[0] - xml_name;
    name = parts[0] + 1;
    name_len = parts[1] - parts[0] - 1;
    if (idx == 3) {
      prefix = parts[1] + 1;
      prefix_len = parts[2] - parts[1] - 1;
    }
  }

  int with_prefix = prefix_len && (top_element || !ns_str);
  xmlns_op res = OP_REPLACE_XMLNS;

  if (state->normalize_ns && !top_element) {
    if (ns_str) {
      if (!state->elements_stack->redefined_top_prefix && prefix_len &&
          has_prefix_ns_from_top(state, prefix, prefix_len, ns, ns_len))
      {
          res = OP_REMOVE_PREFIX;
          with_prefix = 1;
      } else if (same_str_buf(state->elements_stack->namespace_str, ns, ns_len)) {
        res = OP_REMOVE_XMLNS;
        with_prefix = 0;
      }
    }
  } else
    res = OP_NOP;

  if (with_prefix) {
    ASSERT(enif_alloc_binary(name_len + prefix_len + 1, buf));
    memcpy(buf->data, prefix, prefix_len);
    buf->data[prefix_len] = ':';
    memcpy(buf->data + prefix_len + 1, name, name_len);
  } else {
    ASSERT(dup_to_bin(buf, name, name_len));
  }

  if (ns_str) {
    if (top_element && prefix_len > 0)
      *ns_str = NULL;
    else {
      *ns_str = top_element ? dup_buf(ns, ns_len) :
                res == OP_REMOVE_PREFIX ?
                state->elements_stack->namespace_str :
                dup_buf(ns, ns_len);

      if (!*ns_str) {
        enif_release_binary(buf);
        return OP_ERROR;
      }
    }
    if (pfx_str) {
      if (res == OP_REMOVE_PREFIX) {
        *pfx_str = dup_buf(prefix, prefix_len);
        if (!*pfx_str) {
          enif_release_binary(buf);
          if (ns_str && *ns_str)
            enif_free(*ns_str);
          return OP_ERROR;
        }
      } else
        *pfx_str = NULL;
    }
  }

  return res;
}

static ERL_NIF_TERM str2bin(ErlNifEnv *env, const char *s)
{
  return dup_to_term(env, s, strlen(s));
}

static void send_event(state_t *state, ERL_NIF_TERM el)
{
  state->size = 0;
  if (state->gen_server) {
    enif_send(state->env, state->pid, state->send_env,
              enif_make_tuple2(state->send_env,
                               enif_make_atom(state->send_env, "$gen_event"),
                               el));
  } else {
    enif_send(state->env, state->pid, state->send_env, el);
  }
  enif_clear_env(state->send_env);
}

static void send_all_state_event(state_t *state, ERL_NIF_TERM el)
{
  state->size = 0;
  if (state->gen_server) {
    enif_send(state->env, state->pid, state->send_env,
              enif_make_tuple2(state->send_env,
                               enif_make_atom(state->send_env, "$gen_all_state_event"),
                               el));
  } else {
    enif_send(state->env, state->pid, state->send_env, el);
  }
  enif_clear_env(state->send_env);
}

static ERL_NIF_TERM append_attr(state_t *state, ERL_NIF_TERM root, ERL_NIF_TERM name, ERL_NIF_TERM value) {
  ErlNifEnv* env = state->send_env;

  if (state->use_maps) {
    ERL_NIF_TERM res;
    enif_make_map_put(env, root, name, value, &res);
    return res;
  } else {
    return enif_make_list_cell(env, enif_make_tuple2(env, name, value), root);
  }
}

void erlXML_StartElementHandler(state_t *state,
                                const XML_Char *name,
                                const XML_Char **atts)
{
  int i = 0;
  ErlNifEnv* env = state->send_env;
  ERL_NIF_TERM attrs_term;
  ErlNifBinary name_bin;

  if (state->use_maps) {
    attrs_term = enif_make_new_map(env);
  } else {
    attrs_term = enif_make_list(env, 0);
  }

  if (state->error)
    return;

  state->depth++;

  while (atts[i])
    i += 2;

  i -= 2;

  while (i >= 0) {
    ErlNifBinary attr_name;
    ERL_NIF_TERM val;
    unsigned char *val_str;

    PARSER_MEM_ASSERT(encode_name(state, atts[i], &attr_name, NULL, NULL, 0));

    size_t val_len = strlen(atts[i+1]);
    val_str = enif_make_new_binary(env, val_len, &val);
    PARSER_MEM_ASSERT(val_str);
    memcpy(val_str, atts[i+1], val_len);

    attrs_term = append_attr(state, attrs_term, enif_make_binary(env, &attr_name), val);
    i -= 2;
  }

  char *ns = NULL, *pfx = NULL;
  int redefined_top_prefix = state->depth > 1 ? state->elements_stack->redefined_top_prefix : 0;
  int xmlns_op;

  if (state->normalize_ns)
      xmlns_op = encode_name(state, name, &name_bin, &ns, &pfx, state->depth == 1);
  else
    xmlns_op = encode_name(state, name, &name_bin, NULL, NULL, state->depth == 1);

  PARSER_MEM_ASSERT(xmlns_op);

  if (!state->normalize_ns)
    xmlns_op = OP_NOP;

  int non_xmpp_ns = -1;
  int had_stream_stream = 0;

  while (state->xmlns_attrs) {
    ERL_NIF_TERM tuple = 0;
    ERL_NIF_TERM tuple_name = 0, tuple_val = 0;
    attrs_list_t *c = state->xmlns_attrs;
    ErlNifBinary new_prefix, new_ns;

    state->xmlns_attrs = c->next;

    if (state->depth == 1 && state->normalize_ns && c->name.size > 6) {
      if (non_xmpp_ns != 1 || !has_prefix_ns_from_list(&stream_stream_ns_attr,
                                                       (char*)c->name.data+6, c->name.size-6,
                                                       (char*)c->value.data, c->value.size))
      {
        if (had_stream_stream) {
          PARSER_MEM_ASSERT(dup_to_bin(&new_prefix, (char*)stream_stream_ns_attr.name.data,
                                       stream_stream_ns_attr.name.size));
          PARSER_MEM_ASSERT(dup_to_bin(&new_ns, (char*)stream_stream_ns_attr.value.data,
                                       stream_stream_ns_attr.value.size));
          c->name = new_prefix;
          c->value = new_ns;
          c->next = state->top_xmlns_attrs;
          state->top_xmlns_attrs = c;
          had_stream_stream = 0;
        }
        non_xmpp_ns = 1;
        PARSER_MEM_ASSERT(dup_to_bin(&new_prefix, (char*)c->name.data+6, c->name.size-6));
        PARSER_MEM_ASSERT(dup_to_bin(&new_ns, (char*)c->value.data, c->value.size));
      } else {
        had_stream_stream = 1;
        non_xmpp_ns = 0;
      }
    }

    if (c->name.size == 5) { // xmlns
      if (xmlns_op == OP_REMOVE_XMLNS) {
        enif_release_binary(&c->name);
        enif_release_binary(&c->value);
        enif_free(c);
        continue;
      } else if (xmlns_op == OP_REPLACE_XMLNS) {
        enif_release_binary(&c->value);
        if (state->use_maps) {
          tuple_name = enif_make_binary(env, &c->name);
          tuple_val = dup_to_term(env, ns, strlen(ns));
        } else {
          tuple = enif_make_tuple2(env, enif_make_binary(env, &c->name),
                                   dup_to_term(env, ns, strlen(ns)));
        }
        xmlns_op = OP_NOP;
      }
      if (!ns && state->normalize_ns)
        PARSER_MEM_ASSERT(ns = dup_buf((char *) c->value.data, c->value.size));
    } else if (xmlns_op == OP_REMOVE_PREFIX &&
        same_str_buf(pfx, (char*)c->name.data + 6, c->name.size - 6)) {
      enif_release_binary(&c->name);
      enif_release_binary(&c->value);
      enif_free(c);
      continue;
    } else if (!redefined_top_prefix && state->depth > 1 && c->name.size > 6 &&
        has_prefix_ns_from_top(state, (char*)c->name.data + 6, c->name.size - 6, NULL, 0)) {
      redefined_top_prefix = 1;
    }

    if (state->use_maps) {
      if (!tuple_name) {
        enif_make_map_update(env, attrs_term, enif_make_binary(env, &c->name),
                             enif_make_binary(env, &c->value), &attrs_term);
      } else
        enif_make_map_update(env, attrs_term, tuple_name, tuple_val, &attrs_term);
    } else {
      if (!tuple) {
        tuple = enif_make_tuple2(env, enif_make_binary(env, &c->name),
                                 enif_make_binary(env, &c->value));
      }
      attrs_term = enif_make_list_cell(env, tuple, attrs_term);
    }

    if (non_xmpp_ns && state->depth == 1 && state->normalize_ns && c->name.size > 6) {
      c->name = new_prefix;
      c->value = new_ns;
      c->next = state->top_xmlns_attrs;
      state->top_xmlns_attrs = c;
    } else
      enif_free(c);
  }

  if (!non_xmpp_ns && state->depth == 1 && state->normalize_ns) {
    state->top_xmlns_attrs = &stream_stream_ns_attr;
  }

  if (xmlns_op == OP_REPLACE_XMLNS) {
    attrs_term = append_attr(state, attrs_term, dup_to_term(env, "xmlns", 5),
                             dup_to_term(env, ns, strlen(ns)));
  } else if (xmlns_op == OP_REMOVE_PREFIX) {
    enif_free(pfx);
  }

  if (!ns && state->normalize_ns)
    PARSER_MEM_ASSERT(ns = dup_buf("", 0));

  xmlel_stack_t *xmlel = enif_alloc(sizeof(xmlel_stack_t));
  PARSER_MEM_ASSERT(xmlel);

  xmlel->next = state->elements_stack;
  xmlel->attrs = attrs_term;
  xmlel->namespace_str = ns;
  xmlel->children = NULL;
  xmlel->redefined_top_prefix = redefined_top_prefix;

  state->elements_stack = xmlel;

  if (state->pid && state->depth == 1) {
    if (state->use_maps) {
      ERL_NIF_TERM map = enif_make_new_map(env);
      enif_make_map_put(env, map, enif_make_atom(env, "__struct__"),
                        enif_make_atom(env, "Elixir.FastXML.StreamStart"), &map);
      enif_make_map_put(env, map, enif_make_atom(env, "name"),
                        enif_make_binary(env, &name_bin), &map);
      enif_make_map_put(env, map, enif_make_atom(env, "attrs"),
                        attrs_term, &map);
      send_event(state, map);
    } else {
      send_event(state,
                 enif_make_tuple3(env,
                                  enif_make_atom(env, "xmlstreamstart"),
                                  enif_make_binary(env, &name_bin),
                                  attrs_term));
    }
  } else {
    xmlel->name = enif_make_binary(env, &name_bin);
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
    state->elements_stack->children = children;
  }

  return;
}

ERL_NIF_TERM
make_xmlel_children_list(state_t *state, children_list_t *list) {
  ErlNifEnv *env = state->send_env;
  ERL_NIF_TERM children_list = enif_make_list(env, 0);

  while (list) {
    if (list->is_cdata) {
      ERL_NIF_TERM data;
      if (state->use_maps) {
        data = enif_make_binary(env, &list->cdata);
      } else {
        data = enif_make_tuple2(env,
                         enif_make_atom(env, "xmlcdata"),
                         enif_make_binary(env, &list->cdata));
      }
      children_list = enif_make_list_cell(env, data, children_list);
    } else
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

    PARSER_MEM_ASSERT(encode_name(state, name, &name_bin, NULL, NULL, 0));

    if (state->use_maps) {
      ERL_NIF_TERM map = enif_make_new_map(env);
      enif_make_map_put(env, map, enif_make_atom(env, "__struct__"),
                        enif_make_atom(env, "Elixir.FastXML.StreamEnd"), &map);
      enif_make_map_put(env, map, enif_make_atom(env, "name"),
                        enif_make_binary(env, &name_bin), &map);
      send_event(state, map);
    } else {
      send_event(state,
                 enif_make_tuple2(env,
                                  enif_make_atom(env, "xmlstreamend"),
                                  enif_make_binary(env, &name_bin)));
    }
    return;
  }

  ERL_NIF_TERM xmlel_term;

  if (state->use_maps) {
    xmlel_term = enif_make_new_map(env);
    enif_make_map_put(env, xmlel_term, enif_make_atom(env, "__struct__"),
                      enif_make_atom(env, "Elixir.FastXML.El"), &xmlel_term);
    enif_make_map_put(env, xmlel_term, enif_make_atom(env, "name"), state->elements_stack->name, &xmlel_term);
    enif_make_map_put(env, xmlel_term, enif_make_atom(env, "attrs"), state->elements_stack->attrs, &xmlel_term);
    enif_make_map_put(env, xmlel_term, enif_make_atom(env, "children"),
        make_xmlel_children_list(state, state->elements_stack->children), &xmlel_term);
  } else {
    xmlel_term = enif_make_tuple4(env, enif_make_atom(env, "xmlel"),
                                  state->elements_stack->name,
                                  state->elements_stack->attrs,
                                  make_xmlel_children_list(state, state->elements_stack->children));
  }

  if (!state->pid || state->depth > 1) {
    children_list_t *el;
    xmlel_stack_t *cur_el = state->elements_stack;

    PARSER_MEM_ASSERT(el = enif_alloc(sizeof(children_list_t)));

    state->elements_stack = state->elements_stack->next;

    el->is_cdata = 0;
    el->term = xmlel_term;
    el->next = state->elements_stack->children;
    state->elements_stack->children = el;
    if (cur_el->namespace_str != state->elements_stack->namespace_str)
      enif_free(cur_el->namespace_str);
    enif_free(cur_el);
  } else {
    xmlel_stack_t *cur_el = state->elements_stack;
    state->elements_stack = cur_el->next;
    if (!state->elements_stack || cur_el->namespace_str != state->elements_stack->namespace_str)
      enif_free(cur_el->namespace_str);
    enif_free(cur_el);
    if (state->use_maps) {
      enif_make_map_put(env, xmlel_term, enif_make_atom(env, "__struct__"),
                        enif_make_atom(env, "Elixir.FastXML.El"), &xmlel_term);
      send_event(state, xmlel_term);
    } else {
      send_event(state,
                 enif_make_tuple2(state->send_env,
                                  enif_make_atom(state->send_env, "xmlstreamelement"),
                                  xmlel_term));
    }
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

  attrs_list_t *c = enif_alloc(sizeof(attrs_list_t));
  PARSER_MEM_ASSERT(c);

  if (prefix) {
    size_t len = strlen(prefix);

    if (!enif_alloc_binary(len + 6, &c->name)) {
      enif_free(c);
      PARSER_MEM_ASSERT(0);
    }
    memcpy(c->name.data, "xmlns:", 6);
    memcpy(c->name.data + 6, prefix, len);
  } else {
    if (!enif_alloc_binary(5, &c->name)) {
      enif_free(c);
      PARSER_MEM_ASSERT(0);
    }
    memcpy(c->name.data, "xmlns", 5);
  };

  size_t len = strlen(uri);
  if (!enif_alloc_binary(len, &c->value)) {
    enif_release_binary(&c->name);
    enif_free(c);
    PARSER_MEM_ASSERT(0);
  }

  memcpy(c->value.data, uri, len);

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

    enif_release_binary(&c->name);
    enif_release_binary(&c->value);
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
    if (!c->next || c->namespace_str != c->next->namespace_str)
      enif_free(c->namespace_str);
    state->elements_stack = c->next;
    enif_free(c);
  }
  if (state->top_xmlns_attrs != &stream_stream_ns_attr)
    while (state->top_xmlns_attrs) {
      attrs_list_t *c = state->top_xmlns_attrs;
      state->top_xmlns_attrs = c->next;
      enif_release_binary(&c->name);
      enif_release_binary(&c->value);
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

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return load(env, priv, load_info);
}

static ERL_NIF_TERM make_parse_error(ErlNifEnv *env, XML_Parser parser)
{
  enum XML_Error errcode = XML_GetErrorCode(parser);
  const char *errstring;

  if (errcode == XML_ERROR_EXTERNAL_ENTITY_HANDLING)
    errstring = "DTDs are not allowed";
  else
    errstring = XML_ErrorString(errcode);

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

  state->send_env = env;

  xmlel_stack_t *xmlel = enif_alloc(sizeof(xmlel_stack_t));
  if (!xmlel) {
    enif_release_resource(state);
    return enif_make_badarg(env);
  }

  memset(xmlel, 0, sizeof(xmlel_stack_t));

  xmlel->next = state->elements_stack;
  xmlel->children = NULL;

  state->elements_stack = xmlel;

  int res = XML_Parse(state->parser, (char *)bin.data, bin.size, 1);
  if (res == XML_STATUS_OK && state->elements_stack->children &&
          !state->elements_stack->children->is_cdata)
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

static void send_error(state_t *state, ERL_NIF_TERM msg) {
  ErlNifEnv *env = state->send_env;

  if (state->use_maps) {
    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, enif_make_atom(env, "__struct__"),
                      enif_make_atom(env, "Elixir.FastXML.StreamError"), &map);
    enif_make_map_put(env, map, enif_make_atom(env, "desc"),
                      msg, &map);

    send_event(state, map);
  } else {
    send_event(state,
               enif_make_tuple2(env,
                                enif_make_atom(env, "xmlstreamerror"),
                                msg));
  }
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
    size_t size = state->size;
    send_error(state, str2bin(state->send_env, "XML stanza is too big"));
    /* Don't let send_event() to set size to zero */
    state->size = size;
  } else {
    int res = XML_Parse(state->parser, (char *)bin.data, bin.size, 0);
    if (!res)
      send_error(state, state->error ?
                        str2bin(state->send_env, state->error) :
                        make_parse_error(state->send_env, state->parser));
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
  int gen_srv = 1;
  int use_maps = 0;

  if (argc != 2 && argc != 3)
    return enif_make_badarg(env);

  if (argc == 3) {
    if (!enif_is_list(env, argv[2]))
      return enif_make_badarg(env);
    ERL_NIF_TERM head, tail = argv[2];
    while (enif_get_list_cell(env, tail, &head, &tail)) {
      char buf[16];
      if (enif_get_atom(env, head, buf, sizeof(buf), ERL_NIF_LATIN1)) {
        if (strcmp("no_gen_server", buf) == 0)
          gen_srv = 0;
        else if (strcmp("use_maps", buf) == 0)
          use_maps = 1;
      }
    }
  }

  ErlNifPid pid;
  if (!enif_get_local_pid(env, argv[0], &pid))
    return enif_make_badarg(env);

  state_t *state = init_parser_state(&pid);
  if (!state)
    return enif_make_badarg(env);

  state->normalize_ns = 1;
  state->use_maps = use_maps;
  state->gen_server = gen_srv;

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
    {"new", 3, new_nif},
    {"parse", 2, parse_nif},
    {"parse_element", 1, parse_element_nif},
    {"reset", 1, reset_nif},
    {"close", 1, close_nif},
    {"change_callback_pid", 2, change_callback_pid_nif}
  };

ERL_NIF_INIT(fxml_stream, nif_funcs, load, NULL, upgrade, NULL)
