
import tables, strutils, sequtils

import glossolalia


type NK* {.pure.} = enum
  Ident, String, Symbol,
  IntLiteral,
  Message, Return, Array, Block
type 
  Node* = ref NodeObj
  NodeObj* {.acyclic.} = object
    case kind*: NK
    of NK.Ident .. NK.Symbol: str*: string
    of NK.IntLiteral: i*: int
    of NK.Message .. NK.Array:
      sub*: seq[Node]
    of NK.Block:
      args*,locals*:seq[string]
      stmts*: seq[Node]

const valid_operator = {
  '+','-','/','*','&','|','%','$','@','=','<','>'}

proc `[]`* (n:Node; idx:Positive): Node =
  n.sub[idx]

proc lispRepr* (n:Node): string =
  result = "("
  result.add($n.kind)

  case n.kind
  of NK.Ident:
    result.add ' '
    result.add n.str
  of NK.Symbol:
    result.add " #"
    result.add n.str

  of NK.Message .. NK.Array:
    for n in n.sub:
      result.add ' '
      result.add lispRepr(n)
  of NK.IntLiteral:
    result.add ' '
    result.add($n.i)
  of NK.Block:
    result.add ' '
    result.add($n.args)
    result.add ' '
    result.add($n.locals)
    for s in n.stmts:
      result.add ' '
      result.add lispRepr s
  of NK.String:
    result.add " '"
    result.add n.str
    result.add '\''

  result.add ')'

proc `$` * (n:Node): string = 
  case n.kind
  of NK.Message:
    # assert n.sub.len >= 2
    # result = "("
    # result.add($n.sub[0].str)
    # for i in 1 .. high(n.sub):
    #   result.add ' '
    #   result.add($n.sub[i])
    # result.add ')'
    
    let msg = n.sub[0].str
    result = $ n.sub[1]
    if n.sub.len == 2:
      result.add ' '
      result.add msg
    else:

      var start = 0
      var ii= 2
      var idx = msg.find(':', start)
      while idx != -1:
        result.add ' '
        result.add msg.substr(start, idx)
        result.add ' '
        result.add($n.sub[ii])
        start = idx+1
        idx = msg.find(':', start)
        ii += 1



  of NK.Block:
    result = "["
    for arg in n.args:
      result.add ':'
      result.add arg
      result.add ' '
    for local in n.locals:
      result.add local
      result.add ' '
    result.add '|'
    for i,statement in n.stmts:
      result.add "\n"
      result.add($statement)
      if i < high(n.stmts):
        result.add '.'
    result.add ']'

  of NK.Array:
    result = "{"
    for idx,node in pairs(n.sub):
      result.add($node)
      if idx < n.sub.high:
        result.add ". "
    result.add '}'

  of NK.Return: return "^ "& $n.sub[0]
  of NK.IntLiteral: return $n.i
  of NK.Ident: return n.str
  of NK.Symbol: return "#"&n.str
  of NK.String: return "'$#'".format(n.str)


proc parens* (n: Rule[Node]): Rule[Node] =
  charMatcher[Node]({'('}) and n and charMatcher[Node]({')'})

proc Ident* (m:string): Node = Node(kind:NK.Ident, str:m)


proc save_expr_keyword (nodes: seq[Node]): Node =
  var ident = Node(kind:NK.Ident, str:"")
  result = Node(kind: NK.Message, sub: @[ident])
  var i = 0
  if (nodes.len and 1) != 0:
    result.sub.add nodes[0]
    i = 1
  else:
    result.sub.add Ident("thisContext")

  for j in countup(i,high(nodes),2):
    ident.str.add nodes[j].str
    result.sub.add nodes[j+1]

proc save_expr_binary (nodes: seq[Node]): Node =
  result = nodes[0]
  for i in countup(1, nodes.high, 2):
    result = Node(kind: NK.Message, sub: @[nodes[i], result, nodes[i+1]])

proc UnaryExpr* (recv:Node; msgs:seq[Node]): Node =
  result = recv
  for i in 0 .. high(msgs):
    result = Node(kind: NK.Message, sub: @[msgs[i], result])

  # result = Node(kind: NK.Message, sub: @[recv])
  # result.sub.add msgs

proc p (r:Rule[Node]):Rule[Node] =
  Rule[Node](
    m: proc(input:var InputState): Match[Node] =
      result = r.m(input)
      echo input.pos
  )
proc echoP ():Rule[Node] =
  Rule[Node](
    m: proc(input:var InputState): Match[Node] =
      echo input.str[input.pos]
      result = Match[Node](kind:mUnrefined, pos:input.pos,len: -1)
  )

grammar(Node):

  ws := +(newline or chr({' ','\t'}) or comment)
  newline := str("\r\L") or chr({'\L'})
  colon := chr(':')
  double_quote := chr('"')

  comment :=
    double_quote and 
    +(absent(double_quote) and anyChar) and
    double_quote
  ident := ident_str.save(Ident)
  ident_str := chr({'A'..'Z', 'a'..'z', '_'}) and *chr({'A'..'Z','a'..'z','0'..'9','_'})
  keywd := keyword_str.save(Ident)
  keyword_str := ident_str and colon
  symbol := chr('#') and (+keyword_str or ident_str).save do (str: string)->Node: Node(kind:NK.Symbol, str:str)

  binary_op :=
    (+chr(valid_operator)).save(Ident)

  literal_int := 
    chr(strutils.Digits).repeat(1).save do(m:string)->Node: 
      Node(kind:NK.IntLiteral, i:parseInt(m))

  literal_str :=
    single_quote and
    inner_str_char.repeat(0).save((m:string)->Node=>Node(kind:NK.String,str:m)) and
    single_quote
  inner_str_char :=
    single_quote.absent and printable_chr
  printable_chr :=
    chr({'\32' .. '\126'})
  single_quote :=
    chr({'\''})

  stmt_separator :=
    ?ws and chr('.') and ?ws

  lbrace := chr({'{'})
  rbrace := chr({'}'})
  array_literal :=
    (lbrace and ?ws and rbrace).save((start:cstring,len:int)->Node=> Node(kind: NK.Array, sub: @[])) or
    (lbrace and ?ws and 
      expr_keyword.join(stmt_separator) and 
      ?ws and rbrace).save((ns:seq[Node])->Node => Node(kind:NK.Array, sub:ns))

  argument := colon and ident
  anyChar := chr({char.low .. char.high})
  proc saveArrBlank (r:Rule[Node]):Rule[Node] =
    (r.save do (ns:seq[Node])->Node: Node(kind:NK.Array, sub:ns))
    .saveBlank do ->Node: Node(kind:NK.Array, sub:nil)

  arg_list :=
    (argument.join(?ws) or anyChar.present).saveArrBlank and 
    ?ws and
    (ident.join(ws) or anyChar.present).saveArrBlank and
    chr('|')

  block_literal :=
    ( chr('[') and ?ws and ?(arg_list and ?ws) and
      (expr_return or expr_keyword).join(stmt_separator).saveArrBlank and 
      ?ws and chr(']')
    ).save do (nodes:seq[Node])->Node: 
      result = Node(
        kind: NK.Block)
      if nodes.len == 1:
        result.stmts = nodes[0].sub
        result.args = @[]
        result.locals = @[]

      else:
        template strs (n): seq[string] =
          (if n.kind == NK.Array: (if n.sub.isNil: @[] else: n.sub.mapIt(string, it.str)) else: @[])
        result.args  = nodes[0].strs
        result.locals= nodes[1].strs
        result.stmts = nodes[2].sub

  reserved_identifier :=
    (
      (
        str("thisContext") or
        str("nil") or 
        str("true") or
        str("false")
      ) and absent(chr({'A'..'Z','a'..'z','0'..'9','_'}))
    ).save do (m:string) -> Node:
      Node(kind: NK.Ident, str: m)

  expr_terminal :=
    (literal_int and absent(chr({'A'..'Z','a'..'z'}))) or 
    literal_str or
    block_literal or
    array_literal or
    reserved_identifier or
    parens(?ws and expr_keyword and ?ws)
  expr_unary :=
    ((expr_terminal and +(ws and ident and colon.absent)).save do (ns:seq[Node])->Node:
      UnaryExpr(ns[0], ns[1 .. ^1])
    ) or
    ( reserved_identifier.absent and 
      (ident and colon.absent).join(ws).save do (ns: seq[Node])->Node:
      UnaryExpr(Ident("thisContext"), ns)
    ) or 
    expr_terminal

  expr_binary :=
    ( expr_unary.join(?ws and binary_op and ?ws)
    ).save(save_expr_binary) or
    expr_unary
  expr_keyword :=
    (
      (expr_binary and +(?ws and keywd and ?ws and expr_binary)) or
      (keywd and ?ws and expr_binary).join(?ws)
    ).save(save_expr_keyword) or
    expr_binary

  expr_return :=
    chr('^') and ?ws and expr_keyword.save do(ns:seq[Node])->Node:
      Node(kind: NK.Return, sub: ns)

let Expression* = expr_keyword
let Stmts* = ?ws and expr_keyword.join(stmt_separator) and ?ws 

export glossolalia

when isMainModule:
  template test(str,rule): stmt =
    do_assert rule.match(str), "[FAIL] "& astToStr(rule) &" for "&str

  test "1", expr_unary
  test "1 + 1", expr_binary
  test "1 at: 2 put: 3", expr_keyword
  test "1+2 at: 1", expr_keyword

  test "[1]", block_literal
  test "[1+2]", block_literal
  test "[ 1+2 at: 1 ]", block_literal

  test "[a b]", Expression
  echo Expression.match("[false. true]").nodes[0]

  echo Expression.match("[:n a b| n send: a with: b ]")