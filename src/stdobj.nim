
import cmodel, vm


defPrimitiveComponent(Int, int)
defPrimitiveComponent(String, string)


defineMessage(cxInt, "*") do (other):
  let other_int = other.dataVar(int)
  result = asObject(this.dataVar(int) * other_int)
defineMessage(cxInt, "+") do (other):
  let other_int = other.dataVar(int)
  result = asObject(this.asVar(int) + other_int)
defineMessage(cxInt, "-") do (other):
  asObject(this.dataVar(int) - other.dataVar(int))
defineMessage(cxInt, "<") do (other):
  return
    if this.dataVar(int) < other.dataVar(int):
      obj_true
    else:
      obj_false
defineMessage(cxInt, ">") do (other):
  return if this.dataVar(int) > other.dataVar(int): obj_true else: obj_false
defineMessage(cxInt, "<=") do (other):
  return if this.dataVar(int) <= other.dataVar(int): obj_true else: obj_false
defineMessage(cxInt, ">=") do (other):
  return if this.dataVar(int) >= other.dataVar(int): obj_true else: obj_false


defineMessage(cxInt, "print") do:
  result = asObject($ this.asVar(int))

defineMessage(cxString, "print") do: 
  result = self

defineMessage(cxObj, "asString") do:
  result = self.send("print")




# vm accessory types here 

defineMessage(cxStack, "len") do -> Object:
  this.dataPtr(Stack).len.asObject

defineMessage(cxStack, "pop") do -> Object:
  this.dataPtr(Stack).pop



defineMessage(cxStrTab, "at:") 
do (str):
  wdd: echo "strtab access ", str.asString[]
  if this.asVar(StrTab).isNil: return nil

  let s = str.asString
  if s.isNil: return nil

  result = this.dataVar(StrTab)[s[]]

defineMessage(cxStrTab, "at:put:") 
do (str,val):
  let s = str.asString
  if s.isNil: return nil
  if this.dataVar(StrTab).isNil:
    this.dataVar(StrTab) = newTable[string,Object](4)
  this.dataVar(StrTab)[s[]] = val











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

  of NK.Array:
    for s in node.sub:
      builder.compileNode(s)
    builder.pushArray node.sub.len

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

  of NK.Return:
    builder.compileNode node.sub[0]
    builder.ret

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


import options

proc compileNode* (node: seq[Node]): Option[Object] =
  # creates an argumentless CompiledMethod that 
  # can be run with a fake bound component for context
  var builder = initInstrBuilder()
  for i in 0 .. high(node):
    builder.compileNode node[i]
    if i < high(node):
      builder.pop

  let o = aggxCompiledMethod.instantiate
  o.dataVar(CompiledMethod) = 
    initCompiledMethod("anon", builder.done, args=[], locals=[])
  result = some o

proc `>>=`* [A,B] (opt:Option[A]; fn:proc(some:A):Option[B]): Option[B] =
  if opt.isSome: fn opt.unsafeGet else: none B

proc `$`* [A] (opt:Option[A]): string =
  mixin `$`
  result = if opt.isNone: "None" else: $opt.unsafeGet 

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
  ## compiles a string of top-level expressions separated by periods.
  ##
  parseExpressions(str) >>= compileNode

proc newAnonObj* : Object =
  aggregate(cxObj).instantiate

proc executeMethodAnony* (meth:Object): Option[Object] =
  let 
    ctx = 
      createContext(meth, 
        BoundComponent(
          self: aggregate(cxObj).instantiate, 
          comp: cxObj,
          idx: 0
      ) )
    o_exe = executorForContext(ctx)
    exe = o_exe.dataPtr(Exec)
  ctx.dataVar(Context).exec = o_exe
  while exe.isActive:
    #exe.tick
    tick(o_exe)
  result = some exe.result

proc execute* (expresion:string): Option[Object] =
  # let meth = compileExpressions(expresion)
  # if meth.isNone:
  #   echo "failed to compile ", expresion
  #   return
  # result = executeMethodAnony meth.unsafeGet
  result = 
   compileExpressions(expresion) >>= 
    executeMethodAnony
  
proc execute* (expresion:string; do_between:proc(exe:Object)): Option[Object] =
  let meth = compileExpressions(expresion)
  if meth.isNone: return

  let 
    ctx = 
      createContext(meth.unsafeGet,
        BoundComponent(
          self: aggregate(cxObj).instantiate, 
          comp: cxObj,
          idx: 0
      ) )
    o_exe = executorForContext(ctx)
    exe = o_exe.dataPtr(Exec)
  ctx.dataVar(Context).exec = o_exe
  while exe.isActive:
    tick o_exe
    do_between o_exe
  result = some exe.result



defineMessage(cxBlockContext, "doesNotUnderstand:") do (msg):
  ## here I have to create a new context to call dnu.msg OR "doesNotUnderstand:"
  ## sending it to my parent context
  ## 
  let dnu = msg.dataPtr(DNU)
  let thisCtx = self.dataPtr(Context)
  let parent = this.dataPtr(BlockContext).lexicalParent

  proc `$` (s:Object):string = simpleRepr(s)
  #echo msg.dataVar(DNU)

  if not parent.isNil:
    let newCtx = createMethodCallContext(
      context, parent, dnu.msg, dnu.args)
    thisCtx.exec.setActiveContext newCtx

defineMessage(cxMethodContext, "doesNotUnderstand:") do (msg):
  ## here I have to create a new context to call dnu.msg OR "doesNotUnderstand:"
  ## sending it to my parent context
  ## 
  assert self != obj_lobby

  let dnu = msg.dataPtr(DNU)
  let newCtx = createMethodCallContext(
    context, obj_lobby, dnu.msg, dnu.args)

  #self.printComponents
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
    cxStack, cxContext,
    cxObj
  )
  let ctx = result.dataPtr(Context)
  ctx.caller = caller
  ctx.ip = bl.ipStart
  ctx.highIP = bl.ipEnd
  ctx.instrs = bl.meth
  let bp = result.dataPtr(BlockContext)
  bp.lexicalParent = bl.lexicalParent
  bp.owningBlock = blck
  #blck.printComponents


defineMessage(cxBlock, "instantiate") do:
  let ctx = createBlockContext(self, nil)
  return ctx

defineMessage(cxBlockContext, "caller:") do (ctx):
  self.dataPtr(Context).caller = ctx

defineMessage(cxBlockContext, "activate") do:
  context.dataPtr(Context).exec.setActiveContext(self)

defineMessage(cxBlock, "activateOn:") do (caller):
  let ctx = createBlockContext(self, context)
  context.dataPtr(Context).exec.setActiveContext(ctx)


# defineMessage(cxBlock, "value") do:
#   let ctx = createBlockContext(self, context)
#   # let ctx = createContext(msg, bc)
#   if ctx.isNil: 
#     echo "!! failed to create block context"
#     return nil

#   context.dataPtr(Context).exec.setActiveContext(ctx)


defPrimitiveComponent(BoundComponent, BoundComponent)

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

let cxComponent* = slotsComponent("Component")
#defPrimitiveComponent(Component, Component)
# let
#   cxComponent* = typeComponent(Component)
#   aggxComponent* = aggregate(cxComponent, cxObj)
# cxComponent.aggr = aggxComponent

assert aggxStaticComponent.insertBehavior(cxComponent, 0)
assert aggxSlots.insertBehavior(cxComponent, 0)

#defPrimitiveComponent(AggregateType, AggregateType)
defPrimitiveComponent(AggregateType, Aggr)
# let
#   cxAggregateType* = typeComponent(AggregateType)
#   aggxAggregateType* = aggregate(cxAggregateType, cxObj)
# cxAggregateType.aggr = aggxAggregateType

# defineMessage(cxComponent, "defaultAggregateType")
# do: this.dataVar(Component).aggr.asObject




import streams

let
  cxStream* = typeComponent(Stream)
  aggxStream* = aggregate(cxStream, cxObj)
template `aggr=` (co:Object; aggr:Aggr) =
  stdAggr(co) = aggr
cxStream.aggr = aggxStream

defineMessage(cxStream, "print:") do (str):
  this.dataVar(Stream).write(str.asString[])


template newStream (s:Stream): Object =
  var o = aggxStream.instantiate
  o.dataVar(Stream) = s
  o
let
  xStdout* = newStream(newFilestream(stdout))
  xStderr* = newStream(newFilestream(stderr))

defineMessage(cxObj, "printValue") do:
  #self.printComponents
  discard xStdout.send("print:", self.send("print"))
  self



let
  componentDict = obj_lobby.ty.instantiate

# proc getComponentComponent* (name:string): Object =
#   componentDict.dataVar(StrTab)[name]

proc registerComponent (co: Object) =
  if not co.isNil:
    # let obj = aggxComponent.instantiate
    # obj.dataVar(Component) = co
    # discard componentDict.send(
    #   "at:put:", asObject(co.name), obj)
    discard componentDict.send(
      "at:put:", 
      asObject(co[Identifier].string), 
      co)

    echo "### defined ", co[Identifier].string

proc registerNewStaticComponents* =
  var start{.global.} = 0
  echo "### starting from ", start
  for co in cmodel.staticComponentsFrom(start):
    registerComponent co
    start = co[NimDataType].nimType
  #start = cmodel.nextStaticID()
  echo "### ending at ", start

registerNewStaticComponents()
# for component in cmodel.knownStaticComponents():
#   registerComponent component
for component in [cxTrue, cxFalse, cxObj, cxUndef, cxOpenNullarySender, cxBlockContext, cxMethodContext, cxComponent]:
  registerComponent component

for k,v in items({
            "Components": componentDict,
            "stdout": xStdout,
            "stderr": xStderr,
          }):
  discard obj_lobby.send(
    "at:put:", asObject(k), v)



defineMessage(cxMtable, "define:as:") 
do (msg,blck):
  let pBlock = blck.dataPtr(Block)
  assert(not pBlock.isNil)

  # define a CompiledMethod out of the bytecode for this blck
  let method_owner = pBlock.meth
  let len = pBlock.ipEnd - pBlock.ipStart + 1
  let ipSource =
    cast[ptr UncheckedArray[char]](
      method_owner.dataPtr(CompiledMethod).bytecode[0].addr
    )
  var iseq = newSeq[byte](len)
  copyMem iseq[0].addr, ipSource + pBlock.ipStart, len

  # copy out args and locals
  # TODO this sucks, but this only has to happen one other place thats 
  # at block instantiation (stdobj.createBlockContext)
  # instead when a method is created it should include contexts for sub-blocks
  var locals = [
    newSeq[string](pBlock.nargs),
    newSeq[string](pBlock.nlocals)
  ]
  let locals_ip = ipSource + pBlock.argNamesAt
  var tab_id, arg_idx = 0
  if pBlock.nargs == 0: tab_id += 1
  var cur = ""
  for i in 0 .. < pBlock.argNamesBytes:
    let c = locals_ip[i]
    if c == '\00':
      arg_idx += 1
      if arg_idx > locals[tab_id].high:
        tab_id += 1
        arg_idx = 0

    else:
      #echo tab_id,":",arg_idx, "  ", pBlock.nargs, ",",pBlock.nlocals
      locals[tab_id][arg_idx].safeAdd(c)

  let m = msg.asString[]
  result = aggxCompiledMethod.instantiate
  result.dataVar(CompiledMethod) = 
    initCompiledMethod(
      self[Identifier].string & "#" & m,
      iseq,
      args = locals[0],
      locals = locals[1])

  self.rawDefine m, result


defineMessage(cxObj, "findMessage:") do (name):
  let namestr = name.asString
  if namestr.isNil: return
  let (bc,m) = self.findMessage(namestr[])
  if not m.isNil: return m

defineMessage(cxObj, "findComponent:") do (name):
  let n = name.asString
  if n.isNil: return
  result = self.findComponent(n[]).asObject
defineMessage(cxBoundComponent, "setSlot:to:") do (index,val):
  let bc = this.dataPtr(BoundComponent)
  if not bc[].isValid or not bc.comp.hasComponent(cxSlots): 
    return

  var idx = -1
  let i = index.asInt
  if i.isNil:
    let n = index.asString
    if n.isNil:
      return
    idx = bc.comp[Slots].names.find(n[])
    if idx == -1: 
      return

  else:
    idx = i[]
    if idx notIn 0 .. bc.comp[Slots].names.high:
      return

  bc[].slotVar(idx) = val
  result = val

defineMessage(cxBoundComponent, "getSlot:") do (idx):
  let i = idx.asInt
  if i.isNil: return
  let bc = this.dataPtr(BoundComponent)
  if not bc[].isValid or not bc.comp.hasComponent(cxSlots):
    return
  result = bc[].slotVar(i[])

defineMessage(cxPrimitiveMessage, "code") do:
  let cm = this.dataPtr(PrimitiveMessage)
  if cm.code.isNil: return asObject("code is nil?")
  let code = cm.code.replace("\L")
  return asObject(cm.code)
# defineMessage(cxAST, "code") do:
#   let ast = this.dataPtr(Node)
#   if ast.isNil or ast[].isNil: return asObject("nil AST?")
#   return asObject($ ast[])

defineMessage(cxObj, "print") do:
  asObject("($#)".format(self.safeType.printComponentNames(", ")))

defineMessage(cxObj, "numComponents") do:
  asObject(self.safeType.numComponents)


defineMessage(cxContext, "setIP:") do (ip):
  echo "ip = ", self.dataPtr(Context).ip
  echo ip.safeType.printcomponentnames
  this.dataPtr(Context).ip = ip.dataVar(int)
  echo " now ip =  ", ip.dataVar(int)

defineMessage(cxBlockContext, "firstIP") do:
  let owner = this.dataPtr(BlockContext).owningBlock
  result = owner.dataPtr(Block).ipStart.asObject
defineMessage(cxMethodContext, "firstIP") do:
  asObject(0)

defineMessage(cxContext, "highIP") do:
  result = this.dataPtr(Context).highIP.asObject


defineMessage(cxMethodContext, "retry") do:
  let ctx = self.dataPtr(Context)
  if not ctx.isNil:
    ctx.ip = 0
    discard self.dataPtr(Stack).pop
    #echo self.dataPtr(Stack).repr
defineMessage(cxBlockContext, "retry") do:
  let bc = this.dataPtr(BlockContext)
  let ctx = self.dataPtr(Context)
  assert(not ctx.isNil)
  ctx.ip = bc.owningBlock.dataPtr(Block).ipStart

# proc defMetaComponent* (componentName:string): Object =
#   let o = getComponentComponent(componentName)
#   result = slotsComponent(componentName&"-ClassBehavior")
#   assert o.addBehavior(result)


block:
  let CoClsBehav = cxComponent # defMetaComponent("Component")
  defineMessage(CoClsBehav, "newBehavior:") do(name):
    slotsComponent(name.asString[])#.asObject
  defineMessage(CoClsBehav, "withSlots:") do(name, slotsArr):
    let arr = slotsArr.dataPtr(Array)
    if arr.isNil: return

    var slots: seq[string] = @[]
    for itm in arr.elems:
      let s = itm.asString
      if s.isNil: continue

      slots.add s[]

    result = slotsComponent(name.asString[], slots)#.asObject


defineMessage(cxArray, "len") do: 
  asObject(this.dataVar(Array).len)



assert isSome execute(readFile("lib/std.ft"))












