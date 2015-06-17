
import cmodel, vm
export cmodel, vm






import endians

proc serialize* (some:int; builder:var InstrBuilder) =
  let i = builder.index
  builder.addNullBytes sizeof(some)
  let dest = builder.iset[i].addr
  var some = some
  when sizeof(some) == 4:
    bigEndian32(dest, some.addr)
  elif sizeof(some) == 8:
    bigEndian64(dest, some.addr)
  else:
    static: 
      assert false, "wat the size of int is on your masheen omg fix"

proc unserialize* (some:var int; source:RawBytes): int =
  when sizeof(some) == 4:
    bigEndian32(some.addr, source)
    result = 4
  elif sizeof(some) == 8:
    bigEndian64(some.addr, source)
    result = 8




proc serialize* (some:string; builder:var InstrBuilder) =
  assert some.len < uint16.high.int
  builder.write some.len.uint16
  let i = builder.index
  builder.addNullBytes some.len
  copymem builder.iset[i].addr, some.cstring, some.len

#hack
type PtrArr* [T] = ptr UncheckedArray[T]
proc `+`* [T] (some:PtrArr[T], b:int): PtrArr[T] =
  cast[PtrArr[T]](some[b].addr)
template `+=`* (a: var PtrArr; b: int): stmt =
  a = a + b

proc read* (src: var PtrArr[char]; val: var (uint16|int16)): int =
  bigEndian16 val.addr, src
  result = sizeof(uint16)
  src += result
proc read* (src: var PtrArr[char]; nBytes: int; val: var string): int =
  copymem val.cstring, src, nBytes
  result = nBytes
  src += result

proc unserialize* (some:var string; source:RawBytes): int =
  var src = cast[PtrArr[char]](source)
  var strLen: uint16
  result = src.read(strLen)
  if some.isNil: some = newString(strLen.int)
  else: some.setLen(strLen.int)
  result += src.read(strLen.int, some)






defineMessage(cxInt, "loadFromRaw:")
do (byteSrc):
  let dat = byteSrc.dataPtr(RawBytes)
  let my_int = this.asPtr(int)
  let len_read = my_int[].unserialize dat[]
  assert len_read == sizeof(int)
  result = objInt(len_read)

defineMessage(cxString, "loadFromRaw:")
do (byteSrc):
  let dat = byteSrc.dataPtr(RawBytes)
  let my_str = this.dataPtr(string)
  let len_read = my_str[].unserialize dat[]
  assert len_read == (my_str[].len + sizeof(uint16))
  result = objInt(len_read)






import kwdgrammar

# defPrimitiveComponent(ASTNode,Node)

# defineMessage(cxASTNode, "print")
# do:
#   asObject($ this.asVar(Node))





proc compileNode (builder: var InstrBuilder; node: Node) =
  case node.kind
  of NK.IntLiteral:
    builder.pushPOD node.i
  of NK.String:
    builder.pushPOD node.str

  of NK.Message:
    # (msg recv arg0 arg1 ...)
    let msg = node.sub[0].str
    # recv
    builder.compileNode node.sub[1]
    for i in 2 .. high(node.sub):
      # args
      builder.compileNode node.sub[i]
    builder.send msg, len(node.sub)-2


  of NK.Block:
    var new_builder = initInstrBuilder()
    for i,s in node.stmts:
      new_builder.compileNode s
      if i < high(node.stmts): new_builder.pop

    var iseq = new_builder.done
    builder.pushBlock node.args, node.locals, iseq

  of NK.Ident:
    case node.str.toLower
    of "thiscontext":
      builder.pushThisContext
    of "nil":
      builder.pushNil
    of "true":
      builder.pushTrue
    of "false":
      builder.pushFalse

    else:
      echo "what do with ", lisprepr(node)
      quit 1
  else:
    echo "not ready to compile ", lisprepr(node)
    quit 1


proc compileNode* (node: seq[Node]): Object =
  # creates an argumentless CompiledMethod that 
  # can be run with a fake bound component for context
  var builder = initInstrBuilder()
  for i in 0 .. high(node):
    builder.compileNode node[i]
    if i < high(node):
      builder.pop

  result = aggxCompiledMethod.instantiate
  result.dataVar(CompiledMethod) = 
    initCompiledMethod(builder.done, args=[], locals=[])

import options
proc `>>=`* [A,B] (opt:Option[A]; fn:proc(some:A):B): Option[B] =
  if opt.isSome: some fn opt.unsafeGet else: none B
proc `$`* [A] (opt:Option[A]): string =
  mixin `$`
  result = if opt.isNone: "None" else: $opt.unsafeGet 

let Expr = Expression
proc parseExpressions* (str:string): Option[seq[Node]] =
  let m = kwdgrammar.Stmts.match(str)
  if m.kind == mNodes: return some m.nodes

  # var input = InputState(str: str, len: str.len)
  # while true:
  #   let m = Expr.m(input)
  #   if m.kind == mNodes:
  #     assert m.nodes.len == 1
  #     if result.isNone: result = some(@[m.nodes[0]])
  #     else: result.unsafeGet.add m.nodes[0]
  #     discard 
  #   else:
  #     break

proc compileExpressions* (str:string): Option[Object] =
  parseExpressions(str) >>= compileNode



proc execute* (expresion:string): Object =
  let meth = compileExpressions(expresion)
  if meth.isNone:
    echo "failed to compile ", expresion
    return
  let bc = BoundComponent(
    self: aggregate(cxObj).instantiate, 
    comp: cxObj,
    idx: 0
  )
  let ctx = createContext(meth.unsafeGet, bc)
  let o_exe = executorForContext(ctx)
  ctx.dataVar(Context).exec = o_exe
  let exe = o_exe .dataPtr(Exec)
  while exe.isActive:
    #exe.tick
    tick(o_exe)
  result = exe.result


let cxBlockContext = slotsComponent("BlockContext")

defineMessage(cxBlockContext, "doesNotUnderstand:") do (msg):
  ## here I have to create a new context to call dnu.msg OR "doesNotUnderstand:"
  ## sending it to my parent context
  ## 
  let dnu = msg.dataPtr(DNU)
  let thisCtx = self.dataPtr(Context)
  let parent = thisCtx.parent
  if not parent.isNil:
    let newCtx = createMethodCallContext(
      context, parent, dnu.msg, dnu.args)
    thisCtx.exec.setActiveContext newCtx

defineMessage(cxLobbyContext, "doesNotUnderstand:") do (msg):
  ## here I have to create a new context to call dnu.msg OR "doesNotUnderstand:"
  ## sending it to my parent context
  ## 
  assert self != obj_lobby

  let dnu = msg.dataPtr(DNU)
  let newCtx = createMethodCallContext(
    context, obj_lobby, dnu.msg, dnu.args)

  self.printComponents
  self.dataPtr(Context).exec.setActiveContext newCtx




proc createBlockContext (blck, caller:Object): Object =
  let bl = blck.dataPtr(Block)
  let cm = bl.meth.dataPtr(CompiledMethod)
  if bl.isNil or cm.isNil: return nil

  var locals: seq[string] = @[]
  block:
    var idx = cm.bytecode[bl.argNamesAt].addr
    template inc (n:int): stmt =
      idx = cast[ptr byte](cast[int](idx)+n)
    template readU16 : uint16 =
      var res: uint16
      bigEndian16 res.addr, idx
      inc 2
      res

    let nBytes = bl.argNamesBytes
    var currentStr: string = nil
    for i in 0 .. <nBytes:
      let c = idx[]
      if c == 0: 
        if not currentStr.isNil:
          locals.add currentStr
          currentStr = nil
      else:
        if currentStr.isNil:
          currentStr = $char(c)
        else:
          currentStr.add char(c)
      inc 1

  let cxLocals = slotsComponent("Locals", locals)
  result = instantiate aggregate(
    cxLocals, cxBlockContext,
    cxStack, cxContext
  )
  let ctx = result.dataPtr(Context)
  ctx.caller = caller
  ctx.ip = bl.ipStart
  ctx.highIP = bl.ipEnd
  ctx.instrs = bl.meth
  ctx.parent = bl.lexicalParent
  #blck.printComponents


defineMessage(cxBlock, "value") do:
  let ctx = createBlockContext(self, context)
  # let ctx = createContext(msg, bc)
  if ctx.isNil: 
    echo "!! failed to create block context"
    return nil

  context.dataPtr(Context).exec.setActiveContext(ctx)

defineMessage(cxBoundComponent, "self") do: 
  return this.dataPtr(BoundComponent).self

defineMessage(cxContext, "caller") do:
  result = this.dataPtr(Context).caller

defineMessage(cxContext, "return:to:") do (val,toContext):
  let ctx = this.dataPtr(Context)
  let toCtx = toContext.dataPtr(Context)
  #may be needed:
  # if toCtx.exec.isNil: toCtx.exec = ctx.exec
  #TODO quadriple check this section of methods..
  assert ctx.exec == toCtx.exec
  toContext.dataPtr(Stack).push val
  ctx.exec.setActiveContext(toContext)



defineMessage(cxExec, "tick") do:
  # let exe = this.asPtr(Exec)
  # exe.tick
  tick(self)




let cxOpenNullarySender* = slotsComponent("OpenNullarySender")
defineMessage(cxOpenNullarySender, "doesNotUnderstand:") do (msg):
  let dnu = msg.dataPtr(DNU)
  if dnu.args.len == 0:
    # send (self at: msg)
    result = self.send("at:", asObject(dnu.msg))

obj_lobby = aggregate(cxOpenNullarySender, cxStrTab, cxObj).instantiate
discard obj_lobby.send("at:put:", asObject("Lobby"), obj_lobby)



let
  cxComponent* = typeComponent(Component)
  aggxComponent* = aggregate(cxComponent, cxObj)
cxComponent.aggr = aggxComponent

let
  cxAggregateType* = typeComponent(AggregateType)
  aggxAggregateType* = aggregate(cxAggregateType, cxObj)
cxAggregateType.aggr = aggxAggregateType

let
  componentDict = obj_lobby.ty.instantiate
discard obj_lobby.send("at:put:", asObject("Components"), componentDict)

proc registerComponent (co: Component) =
  if not co.isNil:
    let obj = aggxComponent.instantiate
    obj.dataVar(Component) = co
    discard componentDict.send(
      "at:put:", asObject(co.name), obj)
    echo "### defined ", co.name

for component in cmodel.knownStaticComponents():
  registerComponent component
for component in [cxTrue, cxFalse, cxObj, cxUndef, cxOpenNullarySender, cxBlockContext]:
  registerComponent component
