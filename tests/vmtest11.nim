
import stdobj,cmodel
import options,kwdgrammar

defineMessage(cxComponent, "define:as:") 
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
      this.dataVar(Component).name & "#" & m,
      iseq,
      args = locals[0],
      locals = locals[1])

  this.dataVar(Component).rawDefine m, result


defineMessage(cxObj, "findMessage:") do (name):
  let namestr = name.asString
  if namestr.isNil: return
  let (bc,m) = self.findMessage(namestr[])
  if not m.isNil: return m

defineMessage(cxPrimitiveMessage, "code") do:
  let cm = this.dataPtr(PrimitiveMessage)
  if cm.code.isNil: return asObject("code is nil?")
  return asObject(cm.code)
# defineMessage(cxAST, "code") do:
#   let ast = this.dataPtr(Node)
#   if ast.isNil or ast[].isNil: return asObject("nil AST?")
#   return asObject($ ast[])

defineMessage(cxObj, "print") do:
  asObject("($#)".format(self.safeType.printComponentNames(", ")))

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

assert isSome execute("""


Components True define: 'print' as: [
  'true'
].
Components False define: 'print' as: [
  'false'
].
Components Undef define: 'print' as: [
  'nil'
].



Components True define: 'ifTrue:' as: [:block|
  block value
].
Components False define: 'ifTrue:' as: [:block|
  nil
].
Components Undef define: 'ifTrue:' as: [:block|
  nil
].

Components True define: 'ifTrue:else:' as: [:trueBlock :falseBlock|
  trueBlock value
].
Components False define: 'ifTrue:else:' as: [:trueBlock :falseBlock|
  falseBlock value
].
Components Undef define: 'ifTrue:else:' as: [:trueBlock :falseBlock|
  falseBlock value
].




Components True define: 'ifFalse:' as: [:block|
  nil
].
Components False define: 'ifFalse:' as: [:block|
  block value
].
Components Undef define: 'ifFalse:' as: [:block|
  block value
].


Components Block define: 'ifTrue:' as: [:block|
  self value ifTrue: block
].
Components Block define: 'ifTrue:else:' as: [:trueBlock :falseBlock|
  self value ifTrue: trueBlock else: falseBlock
].
Components Block define: 'whileTrue:' as: [:block |
  [
    self value ifFalse: [^ nil].
    block value
  ] loopForever
].

Components Block define:'loopForever' as:[
  self value.
  thisContext retry
].

Components Int define: 'factorial' as: [
  [self < 2] ifTrue: [^ 1].
  ^ self * (self-1) factorial
]

""")

proc main =
  block:
    
    echo "----------------------"

    # var o = execute("Lobby retry")
    # assert o == nil
    let print_backtrace = proc(exe:Object) =
      let e = exe.dataPtr(Exec)
      var ctx = e.activeContext
      var i = 0
      while not ctx.isNil:
        #echo ctx.safetype.printComponentNames 
        echo i,": ", ctx.findComponent("Locals").slotNames
        echo "  ", ctx.dataPtr(Context).instrs.dataPtr(CompiledMethod).name
        ctx = ctx.dataPtr(Context).caller

    discard "[x| x: 0. [x < 2] whileTrue: [x: x + 1]. ^x. 2] value"
    let ss = "[x| x: 0. [x < 2] whileTrue: [x: x + 1]. x printValue] value"

    # let o = execute(
    #   ss).unsafeget
    #   #"").unsafeget#, do_between = print_backtrace).unsafeget
    # #"[x| x:1. x<2 ifTrue: [^true]. ^false] value")
    # o.printcomponents
    # echo o.send("print").asString[]

    let ss2 = "5 factorial"
    let o = execute(ss2).unsafeget
    o.printcomponents
    echo o.send("print").asString[]

    # # should loop forever..
    # o = execute("retry")
    # o.printcomponents
    # echo "RESULT: ", o.asString[]


main()
