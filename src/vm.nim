import 
  cmodel, macros, strutils


template echoCode* (xpr:expr): stmt =
  echo astToStr(xpr),": ",xpr
template echoCodeI* (i=2; xpr:expr): stmt =
  echo repeat(' ',i), astToStr(xpr), ": ", xpr


proc slotsComponent* (name: string; slots: varargs[string]): Object 
  ## depends on opcodes

proc init: void
init()

var 

  cxObj*, cxTrue*, cxFalse*: Object
  objLobby*, objTrue*, objFalse*: Object ## defined later, this is where globals live

  cxUndef*: Object

  cxContext*, cxMethodContext*, cxBlockContext*: Object

  cxExec*: Object
  aggxExec*: Aggr

  cxRawBytes*: Object
  aggxRawBytes*: Aggr

  cxDNU*: Object
  aggxDNU*: Aggr

  cxArray*: Object
  aggxArray*: Aggr

  cxStrTab*: Object
  aggxStrTab*: Aggr


var std_aggrs : seq[Aggr] = @[]
proc stdAggr* (co: Object): var Aggr =
  let nt = co.dataPtr(NimDataType)
  do_assert(not nt.isNil)
  std_aggrs.ensureLen nt.nimType+1
  if std_aggrs[nt.nimType].isNil:
    echo "making new aggr ", co[Identifier].string
    std_aggrs[nt.nimType] = aggregate(co, cxObj)
  return std_aggrs[nt.nimType]
proc stdAggr* (nimType:int): var Aggr =
  do_assert nimType in 0 .. cmodel.nextStaticID()
  std_aggrs.ensureLen nimType+1
  return std_aggrs[nimType]


type
  NimTerminalObject* = concept X
    asObject(X) is Object

template defPrimitiveComponent* (n:untyped, comp:typed): stmt =
    static: assert `comp` isNot NimTerminalObject

    let `cx n`* {.inject} = typeComponent(comp)
    let `aggx n`* {.inject} = aggregate(typeComponent(comp), cx_obj)
    #`cx n`.aggr = `aggx n`
    stdAggr(`cx n`) = `aggx n`
    #`cx n`.name = astToStr(n)
    `cx n`[Identifier] = astToStr(n).Identifier
    proc `obj n`* (some: `comp`): Object  =
      result = `aggx n`.instantiate
      result.dataVar(comp) = some
    proc asObject* (some: `comp`): Object{.inline.}=
      `obj n`(some)

    static: assert `comp` is NimTerminalObject

    const asX = "as"&astToStr(n)
    defineMessage(`cx n`, "as"&astToStr(n))
    do: return self

    proc `as n`* (some:Object): ptr `comp` {.inline.} =
      result = some.dataPtr(`comp`)
      if not result.isNil: return
      #result = some.send(asX).dataVar(`comp`)
      # try to call asX on the object
      # let o = some.send(asX)
      # let dp2 = o.dataPtr(`comp`)
      # if not dp2.isNil: return dp2[]






## message sending

type
  BoundComponent* = object
    self*: Object
    comp*: Object#Component
    idx*: int

## BoundComponent
##   contains three pieces needed to access an instance of a component:
##   the object that owns it, the component itself and the data offset in object.
##   BoundComponent is valid so long as you do not prepend the object type
proc dataPtr* (c:BoundComponent; t:typedesc): ptr t =
  # take unsafe reference to component data. 
  do_assert c.comp[NimDataType].nimType == typeID(t) #.nimType == data_type_id(t)
  let offs = c.self.ty.components[c.idx][0]
  cast[ptr t](c.self.dat[offs].addr)
proc dataVar* (c:BoundComponent; t:typedesc): var t =
  c.asPtr(t)[]

proc asPtr* (c:BoundComponent; t:typedesc): ptr t {.deprecated.} =
  c.dataPtr(t)
proc asVar* (c:BoundComponent; t:typedesc): var t {.deprecated.} =
  c.dataVar(t)


proc getComponent* (some, comp: Object): BoundComponent {.inline.} =
  BoundComponent(
    self: some, 
    comp: comp, 
    idx: some.findComponentIndex(comp)
  )
proc getComponent* (some: Object; n: int): BoundComponent {.inline.} =
  do_assert n in 0 .. some.safeType.components.high
  BoundComponent(self:some, idx:n, comp:some.safeType.components[n][1])

proc isValid* (bc: BoundComponent): bool = bc.idx > -1


proc slotPtr* (bc: BoundComponent; idx: int): ptr Object =
  let offs = bc.self.safeType.components[bc.idx][0]
  return
    addr cast[ptr UncheckedArray[Object]](bc.self.dat[offs].addr)[idx]
proc slotVar* (bc: BoundComponent; idx: int): var Object =
  bc.slotPtr(idx)[]

proc findComponentIndex* (some:Object; name:string): int {.inline.}=
  let ty = some.safeType
  proc `==` (a:ComponentInstance; name:string): bool =
    a.co.hasComponent(cxIdentifier) and a.co[Identifier].string == name
  result = ty.components.find(name)


proc findComponent* (some: Object; name: string): BoundComponent {.inline.}=
  let idx = some.findComponentIndex(name)
  result.self = some
  result.idx = idx
  if idx == -1:
    return
  result.comp = some.safeType.components[idx][1]



proc slotNames* (some: BoundComponent): seq[string] =
  if not some.isValid or 
      some.comp.isNil:
    return @[]
  let slots = some.comp.dataPtr(Slots)
  if slots.isNil:
    return @[]

  return slots.names




type
  MessageSearch* = object
    continueFrom*: int
    msg*: Object
    bound*: BoundComponent

import tables

proc findMessage* (ty: Aggr; msg: string; res: var MessageSearch): bool =
  
  #for i in countdown(res.continueFrom, 0, 1):
  for i in res.continueFrom .. <ty.numComponents:
    let 
      c = ty.components[i]
      m = c[1].messages[msg]
    if not m.isNil:
      res.continueFrom = i+1
      res.msg = m #cast[MessageImpl](m)
      res.bound.comp = c[1]
      res.bound.idx = i
      result = true
      return

type BoundMessage* = (BoundComponent,Object)

proc findMessage* (obj:Object; msg:string): (BoundComponent,Object) =
  let ty = obj.safeType
  var ms: MessageSearch
  ms.continueFrom = 0 #high(ty.components)
  if ty.findMessage(msg, ms):
    ms.bound.self = obj
    result = (ms.bound, ms.msg)
    return
  result[0].idx = -1






var 
  cxBoundComponent*: Object

type
  Block* = object
    ipStart*, ipEnd*: int
    nArgs*, nLocals*: int
    argNamesAt*, argNamesBytes*: int
    meth*, lexicalParent*: Object
var cxBlock*: Object

type
  CompiledMethod* = object
    name*: string
    bytecode*: seq[byte]
    args,locals: seq[string]
    contextCreator*: Object ## component with slots for args and locals to hold state in the context object
var 
  cxCompiledMethod*: Object
  aggxCompiledMethod*: Aggr


proc initCompiledMethod* (name:string; bytecode:seq[byte]; args,locals:openarray[string]=[]): CompiledMethod =
  result = CompiledMethod(
    name: name,
    bytecode:bytecode,
    args: @args,
    locals: @locals
  )
  result.contextCreator = slotsComponent("Locals", result.args & result.locals)



type
  PrimitiveCB = proc(context: Object; this: BoundComponent): Object{.nimcall.}

  PrimitiveMessage* = object
    fn*: PrimitiveCB
    code*,name*: string

var 
  cxPrimitiveMessage*: Object
  aggxPrimitiveMessage*: Aggr
#cxPrimitiveMessage.aggr = aggxPrimitiveMessage



proc newPrimitiveMessage* (args:openarray[string]; name,src:string; fn:PrimitiveCB): Object 


proc rawDefine* (co:Object; msg:string; obj:Object) =
  co.messages[msg] = obj

macro defineMessage* (co,msg,body:untyped):stmt =
  #co:Component; msg:string; body:untyped{nkDo}
  let cs = callsite() 
  # let co = cs[1]
  # let msg = cs[2]
  # let body = cs[3]

  let params = [
    ident"Object", 
    newIdentDefs(ident"context", ident"Object"),
    newIdentDefs(ident"this", ident"BoundComponent")
  ]
  ## the body param is a nnkDo, lets scan the params and turn them into
  ## templates. 
  let new_body = newStmtList()
  var arg_names = newseq[NimNode]()
  new_body.add quote do:
    template self(): Object = this.self 
    template thisComponent(): Object = this.comp
  var arg_idx = 0
  for fp1 in 1 .. len(body.params)-1:
    let p = body.params[fp1]
    for fp2 in 0 .. len(p)-3:
      let name = p[fp2]
      let strname = repr(name)
      new_body.add(quote do:
        let `name` = context.send(`strname`)
        #template `name`: Object = args[`arg_idx`])
      )
      arg_idx += 1
      arg_names.add(newLit($name))
  ## copy the rest of the function into the new stmt list
  body.body.copyChildrenTo new_body

  let new_proc = newProc(
    params = params, 
    body = new_body, 
    proc_type = nnkLambda)
  new_proc.pragma = newTree(nnkPragma, ident"nimcall")

  var co_safe = co
  # if co_safe.typekind == ntyTypedesc:
  #   co_safe = quote do: typeComponent(`co`)

  ## call rawDefine() to store it
  let argNamesNode = newTree(nnkBracket, arg_names)
  # let src = repr(callsite())
  # echo src
  # echo treerepr cs
  let src = new_proc.repr
  result = quote do:
    let m = newPrimitiveMessage(
      `arg_names_node`, astToStr(`co`)&"#"&`msg`, `src`, `new_proc`)
    rawDefine(`co_safe`, `msg`, m)
  when defined(Debug):
    echo repr result



# template defineMessage* (co:Component; msg:string; body:untyped):stmt {.immediate,dirty.} =
#   getAst(defineMessage2(co,msg,body))



proc send* (recv:Object; msg:string; args:varargs[Object]): Object 







import tables

type
  Instr* {.pure.} = enum
    NOP, 
    PushPOD, PushBLOCK, 
    PushNIL, PushThisContext,
    PushTRUE,PushFALSE,
    PushARRAY,
    Pop, Dup, Send, 
    GetSlot, SetSlot, ExecPrimitive, 
    Return

  iseq = seq[byte]

  InstrBuilder* = object
    iset*: iseq
    index*: int
    #labels*: Table[string,int]

proc initInstrBuilder* : InstrBuilder =
  newSeq result.iset, 0
  #result.labels = initTable[string,int]()


type
  RawBytes* = cstring

  Serializable* = concept obj
    var builder: InstrBuilder
    serialize(obj, builder)
    var source: RawBytes
    unserialize(obj, source) is int



proc ensureLen* (some: var seq; len: int) {.inline.}=
  if some.len < len: some.setLen len

template addByte* (i: var InstrBuilder; b: untyped): stmt =
  let byt = byte(b)
  i.iset.ensureLen i.index+1
  i.iset[i.index] = byt
  i.index += 1

template addNullBytes* (i: var InstrBuilder; n: int): stmt =
  i.iset.ensureLen i.index+n
  i.index += n

proc pushArray* (i:var InstrBuilder; len: int) =
  assert len >= 0
  assert len < 256
  i.addByte Instr.PushARRAY
  i.addByte len

proc pushPOD* (i: var InstrBuilder; obj: Serializable) =
  mixin serialize

  let ty = typeComponent(type(obj))
  let id = ty[NimDataType].nimType
  assert id == typeID(type(obj))
  do_assert id < 127, "FIX ASAP" # make this two bytes if it gets big
  do_assert id > 0, "invalid id "& $id & "::"& ty[Identifier].string
  
  i.addByte Instr.PushPOD
  i.addByte id
  serialize(obj, i)

import endians

proc write* (i: var InstrBuilder; some: uint32|int32) =
  let start = i.index
  i.addNullBytes sizeof(some)
  var some = some
  bigEndian32 i.iset[start].addr, some.addr
proc write* (i: var InstrBuilder; some: uint16|int16) =
  let st = i.index
  i.addNullBytes sizeof(some)
  var some = some
  bigEndian16 i.iset[st].addr, some.addr

proc pushBlock* (i:var InstrBuilder; 
      args,locals:openarray[string]; 
      iseq: var iseq) =

  i.addByte Instr.PushBLOCK
  i.addByte args.len
  i.addByte locals.len
  let localsSizeIDX = i.index
  i.addNullBytes sizeof(uint16)

  for s in args:
    let start = i.index
    i.addNullBytes s.len+1
    copyMem i.iset[start].addr, s.cstring, s.len
  for s in locals:
    let start = i.index
    i.addNullBytes s.len+1
    copyMem i.iset[start].addr, s.cstring, s.len

  var localsSizeU16 = uint16(i.index - localsSizeIDX - sizeof(uint16))
  bigEndian16 i.iset[localsSizeIDX].addr, localsSizeU16.addr

  let L = iseq.len
  i.write uint32(L)
  let start = i.index
  i.addNullBytes L
  copyMem i.iset[start].addr, iseq[0].addr, L



proc pushThisContext* (i: var InstrBuilder) =
  ## stack effect ( -- object )
  i.addByte Instr.PushThisContext

proc pushNIL* (i: var InstrBuilder) =
  ## stack effect ( -- nil )
  i.addByte Instr.PushNIL
proc pushTRUE* (i: var InstrBuilder) =
  i.addByte Instr.PushTRUE
proc pushFALSE* (i: var InstrBuilder) =
  i.addByte Instr.PushFALSE

proc dup* (i: var InstrBuilder) =
  ## stack effect ( object -- object object )
  i.addByte Instr.Dup

proc pop* (i: var InstrBuilder) =
  ## stack effect ( object -- )
  i.addByte Instr.Pop

proc ret* (i: var InstrBuilder) =
  ## stack effect ( object -- )
  i.addByte Instr.Return

proc send* (i: var InstrBuilder; msg:string; args:int) =
  let L = msg.len
  i.addByte Instr.Send
  i.addByte args
  i.addByte L
  let IDX = i.index
  i.addNullBytes L
  copyMem i.iset[IDX].addr, msg.cstring, L

proc getSlot* (i:var InstrBuilder; slot:int) =
  ## stack effect ( -- object )
  assert slot in 0 .. 127 # arbitrary limit
  i.addByte Instr.GetSlot
  i.addByte slot
proc setSlot* (i:var InstrBuilder; slot:int) =
  ## stack effect ( object -- )
  assert slot in 0 .. 127
  i.addByte Instr.SetSlot
  i.addByte slot


proc execPrimitive* (i:var InstrBuilder) =
  i.addByte Instr.ExecPrimitive


proc done* (i: var InstrBuilder): seq[byte] =
  i.iset.setLen i.index
  result = i.iset







proc newPrimitiveMessage* (args:openarray[string]; name,src:string; fn:PrimitiveCB): Object =
  # echoCode aggxPrimitiveMessage.printComponentNames()
  # echoCode name
  # echoCode src
  # echoCode aggxPrimitiveMessage.isNil
  # echoCode aggxPrimitiveMessage.components
  result = aggxPrimitiveMessage.instantiate
  result.dataPtr(PrimitiveMessage).fn = fn
  result.dataPtr(PrimitiveMessage).code = src
  var ibuilder = initInstrBuilder()
  ibuilder.execPrimitive
  let cm = result.dataPtr(CompiledMethod)
  cm[] = initCompiledMethod(name, ibuilder.done, args=args, locals=[])




## dynamic components

var readers: seq[Object] = @[]
proc readSlot (idx:int): Object =
  # returns a CompiledMethod to read slot idx from a component instance

  if idx < 0: return
  if idx < readers.len:
    result = readers[idx]
    if not result.isNil: return
  elif readers.high < idx:
    readers.setLen idx+1

  result = aggxCompiledMethod.instantiate
  readers[idx] = result

  var ibuilder = initInstrBuilder()
  ibuilder.getSlot(idx)

  result.dataVar(CompiledMethod) = 
    initCompiledMethod(
      "slotReader#"& $idx,
      ibuilder.done,
      args=[] )


var writers: seq[Object] = @[]
proc writeSlot (idx:int): Object =
  # return proc(this:BoundComponent; args:varargs[Object]):Object =
  #   let offs = this.offs + (idx * sizeof(pointer))
  #   cast[var Object](this.self.dat[offs].addr) = args[0]

  if idx < 0: return
  if idx < writers.len:
    result = writers[idx]
    if not result.isNil: return
  elif writers.high < idx:
    writers.setLen idx+1

  result = aggxCompiledMethod.instantiate
  writers[idx] = result

  var ib = initInstrBuilder()
  ib.pushThisContext
  ib.send "val", 0
  ib.setSlot(idx)

  result.dataVar(CompiledMethod) = 
    initCompiledMethod(
      "slotWriter#"& $idx,
      ib.done,  args=["val"]  )

proc slotsComponent (name: string; slots: varargs[string]): Object =
  result = aggxSlots.instantiate
  result[Slots].names = @slots
  result[MTable].entries = initTable[string,Object]()
  result[Identifier] = name.Identifier
  for i in 0 .. high(slots):
    let
      m_reader = slots[i]
      m_writer = m_reader&":"
    result.rawDefine m_reader, readSlot(i)
    result.rawDefine m_writer, writeSlot(i)

  # result = Component(
  #   bytes: slots.len * sizeof(pointer),
  #   name: name,
  #   messages: initTable[string,Object](),
  #   kind: ComponentKind.Dynamic,
  #   slots: @slots
  # )
  # for i in 0 .. high(slots):
  #   let 
  #     m_reader = slots[i]
  #     m_writer = m_reader&":"
  #   result.rawDefine m_reader, readSlot(i)
  #   result.rawDefine m_writer, writeSlot(i)






type Stack* = distinct seq[Object]
var 
  cxStack*: Object
  aggxStack*: Aggr

proc len* (some: ptr Stack): int =
  result = 
    if some.isNil: 0
    elif seq[Object](some[]).isNil: 0
    else: seq[Object](some[]).len
proc pop* (some: ptr Stack): Object =
  result = 
    if some.len == 0: nil
    else: seq[Object](some[]).pop
proc push* (some: ptr Stack; val:Object) =
  if some.isNil: return
  if seq[Object](some[]).isNil: 
    newSeq seq[Object](some[]), 0
  seq[Object](some[]).add val

proc top* (some: ptr Stack): Object =
  let some = (ptr seq[Object])(some)
  if some.isNil or some[].isNil or some[].len == 0: return
  return some[][some[].high]

type 
  Context* = object
    caller*: Object # 
    instrs*: Object ## CompiledMethod where instructions live
    exec*: Object ## owner Exec 
    ip*, highIP*: int

  BlockContext* = object
    lexicalParent*, owningBlock*: Object


proc createContext* (compiledMethod:Object; bound:BoundComponent): Object =
  ## allocates a context for a method
  ## does not set Context.caller or .parent
  let cm = compiledMethod.dataPtr(CompiledMethod)
  #echo cm.contextCreator.isNil
  result = instantiate aggregate(
    cm.contextCreator, 
    cxMethodContext,
    cxBoundComponent, 
    cxStack, cxContext,
    cxObj
  )
  result.dataVar(Context).highIP = cm.bytecode.high
  result.dataVar(Context).instrs = compiledMethod
  #result.dataVar(Context).parent = obj_lobby
  result.dataVar(BoundComponent) = bound




# defineMessage(cxCompiledMethod, "createContext") do:
#   let cm = this.asPtr(CompiledMethod)
#   if cm.contextCreator.isNil: return
#   result = aggregate(cxBoundComponent, cm.contextCreator, cxContextInstance)
#     .instantiate
#   discard context.send(result, "parent:", context)


type Exec* = object
  activeContext*: Object #,rootContext*: Object
  bytePtr*: Object
  result*: Object


proc isActive* (some: ptr Exec): bool =
  not some.activeContext.isNil

proc contextIsFinished* (some: ptr Exec): bool =
  let ac = some.activeContext
  if ac.isNil: return true
  let ctx = ac.dataPtr(Context)
  if ctx.isNil: return true
  if ctx.ip > ctx.highIP:
    return true

proc ptrToBytecode* (some: ptr Exec): ptr UncheckedArray[byte] =
  # provides a ptr to bytecode 0 of the active context's bytecode loc
  # bytecode always lives in some method

  let ctx = some.activeContext.dataPtr(Context)
  let cm = ctx.instrs
  # result = cast[ptr UncheckedArray[byte]](
  #   cm.dataPtr(CompiledMethod).bytecode[ctx.ip].addr
  # )
  result = cast[ptr UncheckedArray[byte]](
    cm.dataPtr(CompiledMethod).bytecode[0].addr
  )

proc setActiveContext* (someExec, ctx: Object) =
  if someExec.isNil:
    discard#printComponents ctx

  assert(not someExec.isNil)
  someExec.dataVar(Exec).activeContext = ctx
  if not ctx.isNIL:
    ctx.dataVar(Context).exec = someExec


template wdd * (body:stmt):stmt =
  when defined(Debug): body

const ShowInstruction = 
  defined(Debug) or defined(ShowInstruction)




type Array* = object
  elems*: seq[Object]
#cxArray.aggr = aggxArray

proc len* (some:Array): int = some.elems.len




type 
  StrTab* = TableRef[string,Object]

#cxStrTab.aggr = aggxStrTab


type DNU* = object
  msg*: string
  args*: seq[Object]
  caller*: Object

#cxDNU.aggr = aggxDNU

proc newDNU* (msg:string, args:seq[Object], caller:Object): Object =
  result = aggxDNU.instantiate
  result.dataVar(DNU) = DNU(msg: msg, args: args, caller: caller)

proc simpleRepr* (o:Object): string = "($#)" % o.safeType.printComponentNames(",")

proc createMethodCallContext* (caller, recv:Object; msgName:string; args:seq[Object]): Object =
  ## sets up a method call context to call `msgname` on `recv`
  ## if `msgname` isnt found then `doesNotUnderstand:` is invoked
  ## 
  let (bc,msg) = recv.findMessage(msgName)
  if msg.isNil:

    let (bc,msg) = recv.findMessage("doesNotUnderstand:")
    if msg.isNil:
      return

    when ShowInstruction:
      echo " dnu obj: $# msg: $#".format(recv.simpleRepr, msgName)

    result = createContext(msg,bc)
    # # TODO ctx.ty.components should be reordered
    # result.getComponent(result.ty.components.high).slotVar(0) = 
    #   newDNU(msgName, args, caller)
    result.getComponent(0).slotVar(0) = newDNU(
      msgName, args, caller)

  else:
    result = createContext(msg,bc)
    let locals = result.getComponent(0)#result.ty.components.high)
    for i in 0 .. high(args):
      locals.slotVar(i) = args[i]
  
  result.dataPtr(Context).caller = caller


type VMException* = object of Exception
proc vmException* (strs: varargs[string]) =
  raise newException(VMException, strs.join(""))


proc tick* (self: Object) = 
  let exe = self.dataPtr(Exec)
  assert(not exe.activeContext.isNil)

  let activeContext = exe.activeContext
  let thisContext = activeContext.dataPtr(Context)
  let thisStack = activeContext.dataPtr(Stack)

  wdd:
    echo thisContext.ip, "/", thisContext.highIP

  if exe.contextIsFinished:
    wdd: echo "Leaving context - finished"

    let val = thisStack.pop
    let next = activeContext.dataPtr(Context).caller

    if next.isNil or next.dataPtr(Context).exec != self:
      exe.result = val
      exe.activeContext = nil
      return

    next.dataPtr(Stack).push val
    self.setActiveContext next
    return

  # exe.activeContext = next
  # if not next.isNil:
  #   next.dataPtr(Stack).push val
  # else:
  #   exe.result = val
  # return


  template top (): expr = 
    thisStack.top
  template push (o): stmt =
    thisStack.push o
  template pop (): expr = 
    thisStack.pop


  template readU8 (): uint8 =
    var res = uint8(iset[idx])
    idx += 1
    res
  template readU16 (): uint16 =
    var res: uint16
    bigEndian16(res.addr, iset[idx].addr)
    idx += 2
    res
  template readU32 (): uint32 =
    var res: uint32
    bigEndian32(res.addr, iset[idx].addr)
    idx += 4
    res

  let iset = exe.ptrToBytecode
  var idx = thisContext.ip

  let op = iset[idx]
  when ShowInstruction: 
    echo op.Instr ," @ ", thisContext.ip,"/",thisContext.highIP

  case op.Instr
  
  of Instr.NOP:
    idx += 1

  of Instr.Dup:
    idx += 1
    push top()

  of Instr.Pop:
    idx += 1
    discard pop()

  of Instr.PushFALSE:
    idx += 1
    push obj_false
  of Instr.PushTRUE:
    idx += 1
    push obj_true

  of Instr.PushNIL:
    idx += 1
    push nil.Object

  of Instr.PushThisContext:
    idx += 1
    push activeContext

  of Instr.PushARRAY:
    idx += 1
    let L = readU8().int
    var arr = aggxArray.instantiate
    let arrP = arr.dataPtr(Array)
    arrP.elems.newSeq L
    for i in 1 .. L:
      arrP.elems[L-i] = pop()
    push arr

  of Instr.Return:
    idx += 1
    let obj = pop()
    # search for .parent until we find a method

    proc methodOwner (ctx: Object): Object =
      result = ctx
      while true:
        # echo ".", result.dataPtr(Context).instrs.dataPtr(CompiledMethod).name
        # echo " ", result.dataPtr(Context).ip, " ($#)" % result.safetype.printComponentNames(",")
        # echo "  ",result.findComponentIndex(cxMethodContext)
        # proc `$` (co:Object): string = co[Identifier].string
        # echo result.safeType.components
        if result.findComponentIndex(cxMethodContext) != -1:
          # echo " this is a MethodContext"
          return result
        result = result.dataPtr(BlockContext).lexicalParent

        # var bc = result.dataPtr(BlockContext)
        # if bc.isNil or bc.lexicalParent.isNil: 
        #   assert(result.safeType.findComponentIndex(cxMethodContext) != -1)
        #   break
        # result = bc.lexicalParent
        # # if result.safeType.findComponentIndex(cxMethodContext) != -1:
        # #   return

      # var par = ctx.dataPtr(Context).parent
      # while not par.isNil:
      #   result = par
      #   par = result.dataPtr(Context).parent
    let next = methodOwner activeContext
    let caller = next.dataPtr(Context).caller

    assert next.dataPtr(Context).exec == self
    assert caller != nil
    assert caller != activeContext
    # let caller = next.dataPtr(Context).caller
    # assert caller.dataPtr(Context).exec == self

    # echo "^ return ", obj.safeType.printComponentNames()," to ", 
    #   next.dataPtr(Context).instrs.dataPtr(CompiledMethod).name
    
    # caller.dataPtr(Stack).push(obj)
    # self.setActiveContext(caller)
    caller.dataPtr(Stack).push(obj)
    #next.dataPtr(Context).ip = next.dataPtr(Context).highIP+1
    self.setActiveContext(caller)


  of Instr.ExecPrimitive:
    idx += 1
    # execute the primitive attached to the currently running context
    let pm = thisContext.instrs.dataPtr(PrimitiveMessage)
    when ShowInstruction:
      echo "  ExecPrimitive($#)".format(thisContext.instrs.dataPtr(CompiledMethod).name)
    if not pm.isNil:
      let bc = activeContext.dataPtr(BoundComponent)
      if bc.isNil:
        echo "NO BOUND COMPONENT!"
        push nil
      else:
        if pm.fn.isNil: 
          echo "PRIMITIVE IS NIL??"
          #activeContext.printcomponents
          push nil
        else:
          #activeContext.printcomponents
          let res = pm.fn(activeContext, bc[])
          push res
    else:
      push nil

  of Instr.GetSlot:
    idx += 1
    let slot = iset[idx]
    when ShowInstruction:
      echo "        ", slot
    idx += 1
    let bm = activeContext.dataPtr(BoundComponent)

    if not bm.isNil:

      # found bound method, trying to get slot "slot"
      do_assert slot.int in 0 .. high(bm.comp[Slots].names)#.slots)
      let obj = bm[].slotVar(slot.int)
      #wdd: obj.printcomponents
      push obj

      # let offs = bm.self.ty.components[bm.idx][0]
      # do_assert offs != -1
      # let dp = cast[ptr UncheckedArray[Object]](bm.self.dat[offs].addr)
      # push dp[slot]

    else:
      echo "BoundComponent not found!"
      push nil.Object

  of Instr.SetSlot:
    idx += 1
    let slot = iset[idx]
    idx += 1
    when ShowInstruction:
      echo "SetSlot(", slot, ")"
    let val = pop()
    let bc = activeContext.dataPtr(BoundComponent)
    if not bc.isNil:
      do_assert slot.int in 0 .. high(bc.comp[Slots].names)#.slots)
      do_assert bc[].isValid
      bc[].slotVar(slot.int) = val
      # let offs = bm.self.ty.components[bm.idx][0]
      # do_assert offs != -1
      # let dp = cast[ptr UncheckedArray[Object]](bm.self.dat[offs].addr)
      # dp[slot] = val
    else:
      echo "BoundComponent not found for set slot!!!"

  of Instr.PushPOD:

    idx += 1
    let id = iset[idx].int
    let ty = typeComponent(id)
    idx += 1
    let cstr = cast[cstring](iset[idx].addr)
    #let src = objRawBytes(cstr)
    if exe.bytePtr.isNil:
      exe.bytePtr = aggxRawBytes.instantiate
    let src = exe.bytePtr
    src.dataVar(RawBytes) = cstr

    when defined(Debug):
      echoCodeI 2, ty[Identifier].string#name

    let obj = ty.stdAggr.instantiate()
    if obj.isNil:
      vmException("NEW POD ", ty[Identifier].string, " FAILED  TO LOAD")

    let o2 = obj.send("loadFromRaw:", src)
    let L  = o2.dataPtr(int)
    if L.isNil: echo "LOADFROMRAW: RESULT IS NOT INT!"
    else: 
      idx += L[]

    push obj
    #wdd: echo "  PushPOD($#)" % obj.send("print").asString[]

  of Instr.PushBLOCK:


    idx += 1
    let nArgs = readU8.int
    let nLocals = readU8.int

    let nBytesForLocalNames = readU16.int
    let idxForLocalNames = idx

    idx += nBytesForLocalNames
    let bytecodeLen = readU32.int

    let idxStart = idx
    let idxEnd = idx + bytecodeLen 

    idx += bytecodeLen 

    ## create an object that can create this block's context
    let obj = aggregate(cxBlock, cxObj).instantiate
    let bp = obj.dataPtr(Block)
    bp.ipStart = idxStart
    bp.ipEnd = idxEnd-1
    bp.nArgs = nArgs
    bp.nLocals = nLocals
    bp.argNamesAt = idxForLocalNames
    bp.argNamesBytes = nBytesForLocalNames
    bp.meth = thisContext.instrs
    bp.lexicalParent = activeContext
    push obj
    

  of Instr.Send:
    idx += 1
    let argN = iset[idx].int
    idx += 1
    let L = iset[idx].int
    idx += 1
    var str = newString(L)
    copyMem str[0].addr, iset[idx].addr, L
    idx += L

    when ShowInstruction:
      echo "Send(", str, ", ", argN, ")"
      echo "-- entering ", str

    var args = newSeq[Object](argN)

    #echoCode str

    let H = <argN
    for i in 0 .. H:
      args[H-i] = pop()

    let recv = pop()

    let ctx = createMethodCallContext(
      activeContext, recv, str, args )
    if ctx.isNil:
      # TODO replace with exception ? 
      #recv.printComponents
      echo "doesNotUnderstand missing! fail execution. haha."
      echo "  msg was ", str
      echo "  recv was ", recv.send("print").dataPtr(string)[]
      recv.printComponents
    self.setActiveContext ctx

  else:
    
    echo "unknown opcode ", op.ord, " (", op.Instr , ")"
    quit 1

  thisContext.ip = idx

  when defined(Debug) or defined(ShowStack):
    echo op.Instr, " next IP here: ", thisContext.ip, "/", thisContext.highIP
    echo "stack.len = ", thisStack.len












proc executorForContext* (ctx:Object): Object =
  result = aggxExec.instantiate
  #result.dataPtr(Exec).rootContext = ctx
  result.dataPtr(Exec).activeContext = ctx


proc send* (recv:Object; msg:string; args:varargs[Object]): Object =

  # instantiate the context
  let ctx = 
    createMethodCallContext(
      nil, recv, msg, @args)
  if ctx.isNil: return

  # let (bc,msg) = recv.findMessage(msg)
  # if not bc.isValid: return

  # # validate that msg has args.len args
  # if msg.dataPtr(CompiledMethod).args.len != args.len:
  #   return nil


  # # instantiate the context
  # let ctx = createContext(msg, bc)
  # if ctx.isNil:
  #   echo "ctx is nil??"

  # # set the argument in the slots of the context ._.
  # if args.len > 0:
  #   let idx = ctx.ty.components.high
  #   let (offs, comp) = ctx.ty.components[idx]
  #   let nSlots = comp.slots.len
  #   if nSlots < args.len: return nil
  #     # not enough slots to hold arguments? check here could be smarter
    
  #   let slots = cast[ptr UncheckedArray[Object]](ctx.dat[offs].addr)
  #   for i in 0 .. args.high:
  #     slots[i] = args[i]

  # instantiate and execute a vm
  #var ticks = 0
  let o = executorForContext(ctx)
  ctx.dataPtr(Context).exec = o
  let exe = o.dataPtr(Exec)
  while exe.isActive:
    o.tick
    #ticks += 1
  result = exe.result
  #wdd: echo "TICKS: ", ticks






proc init = 


  cxCompiledMethod = typeComponent(CompiledMethod)
  cxPrimitiveMessage = typeComponent(PrimitiveMessage)
  cxStack   = typeComponent(Stack)
  cxExec = typeComponent(Exec)
  cxRawBytes = typeComponent(cstring)
  cxStrTab = typeComponent(StrTab)
  cxBlock = typeComponent(Block)
  cxArray = typeComponent(Array)
  cxDNU = typeComponent(DNU)
  cxContext = typeComponent(Context)
  cxBlockContext = typeComponent(BlockContext)

  cxBoundComponent = typeComponent(BoundComponent)

  cxObj = slotsComponent("Object")
  cxUndef = slotsComponent("Undef")

  cmodel.aggxUndef = aggregate(cxUndef, cxObj)
  cxMethodContext = slotsComponent("MethodContext")
 
  cxTrue  = slotsComponent("True")
  objTrue = aggregate(cxTrue, cxObj).instantiate

  cxFalse  = slotsComponent("False")
  objFalse = aggregate(cxFalse, cxObj).instantiate



  aggxCompiledMethod = aggregate(cxCompiledMethod, cxObj)

  aggxPrimitiveMessage = aggregate(
    cxPrimitiveMessage, cxCompiledMethod, cxObj)

  aggxStack = aggregate(cxStack, cxObj)

  aggxExec = aggregate(cxExec, cxObj)

  aggxRawBytes = aggregate(cxRawBytes, cxObj)

  aggxStrTab = aggregate(cxStrTab, cxObj)


  aggxArray = aggregate(cxArray, cxObj)

  aggxDNU = aggregate(cxDNU, cxObj)