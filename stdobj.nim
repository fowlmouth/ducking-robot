
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

defineMessage(cxInt, "loadFromRaw:")
do (byteSrc):
  let dat = byteSrc.dataPtr(RawBytes)
  let my_int = this.asPtr(int)
  let len_read = my_int[].unserialize dat[]
  assert len_read == sizeof(int)
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
    echo "not ready to compile ", node
    quit 1


proc compileNode* (node: Node): Object =
  # creates an argumentless CompiledMethod that 
  # can be run with a fake bound component for context
  var builder = initInstrBuilder()
  builder.compileNode node

  result = aggxCompiledMethod.instantiate
  result.dataVar(CompiledMethod) = 
    initCompiledMethod(builder.done, args=[], locals=[])


let Expr = Expression()
proc compileExpr* (str:string): Object =
  let match = Expr.match(str)
  if match.kind != mNodes: 
    echo match
    return

  assert match.nodes.len == 1
  let node = match.nodes[0]
  echo node

  result = compileNode(node)



proc execute* (expresion:string): Object =
  let meth = compileExpr(expresion)
  if meth.isNil:
    echo "failed to compile ", expresion
    return
  let ctx = createContext(meth, BoundComponent(self: nil, idx: 1))
  let o_exe = executorForContext(ctx)
  ctx.dataVar(Context).exec = o_exe
  let exe = o_exe .dataPtr(Exec)
  while exe.isActive:
    #exe.tick
    tick(o_exe)
  result = exe.result


let cxBlockForward = slotsComponent("BlockForward")

defineMessage(cxBlockForward, "doesNotUnderstand:") do (msg):
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

defineMessage(cxLobbyForward, "doesNotUnderstand:") do (msg):
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
    cxLocals, cxBlockForward,
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


defineMessage(cxContext, "caller") do:
  result = this.asPtr(Context).caller

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