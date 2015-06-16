## this is an component composition framework, it is a basic object model
## founded on composition rather than inheritance. 
##
## changes since entoody:
## * removed unicast/multicast messages
##   send() sends a message to the first component that applies,
##   multicast() is an iterator that sends a message to all components that apply
##
## * removed static component ordering.
##   order matters! higher components can intercept messages before
##   lower ordered components
##
## * components are geared towards behavior, nice data structs have _accessors_
##
## What the hell is this though?
## * components are chunks of data and/or behavior. isolation of the two is preferred
##   except when the data is a primitive (nim type)
##   components can be primitive data or dynamic slotted objects
##   their memory layout is final after creation
## * aggregates are collections of components, new behavior is bolted on and overrides
##   old behavior. aggregate types may not add data after they are created, this would
##   invalidate all of their children. a way around this is shown in stdobj.nim
## * objects are instances of aggregates, components here can be added and removed at
##   will only require reallocation if data components are added/removed
## 
##  Example components:
##    Position, Rotation, 
##    TextureRef, 
##    HealthPoints, Energy, MaxLifetime
##    
##
##
##
##
##

import typetraits,tables,macros,strutils
export macros

type
  NimDataTypeID* = int

  ComponentKind* {.pure.}= enum
    Static, Dynamic
  Component* = ref object
    bytes*: int
    name*: string
    messages*: Table[string,Object]

    case kind*: ComponentKind
    of ComponentKind.Static:
      nimType*: NimDataTypeID
      destructor, initializer: ObjectDestructor
      aggr*: AggregateType

    of ComponentKind.Dynamic:
      slots*: seq[string]


  AggregateType* = ref object
    bytes*: int
    components*: seq[(int,Component)]

  UncheckedArray* {.unchecked.}[T] = array[1,T]
  Object* = ref object
    ty*: AggregateType
    dat*: UncheckedArray[byte]

  ObjectDestructor* = proc(self:Object, componentData: pointer){.nimcall.}


var data_type_counter{.global.} = 1
var staticComponents{.global.}: seq[Component] = @[]

iterator knownStaticComponents* : Component = 
  for i in 1 .. high(staticComponents):
    yield staticComponents[i]

proc castTo (fro,to:NimNode): NimNode {.compileTime.}=
  newTree(nnkCast, to, fro)

proc genDecrefStmts (ty:NimNode, data:NimNode): NimNode {.compileTime.} =
  result = newStmtList()
  let stmts = newStmtList()
  var skipCast = false

  case ty.typekind
  of ntyInt:
    skipCast = true


  of ntyString:
    let data = data.castTo(newTree(nnkPtrTy, ty))
    result = quote do: 
        `data`[] = nil
        # let `strdata` = 
        # if not `data`[].isNil: GCunref(`data`[])

  of ntyObject:
    echo treerepr data
    let data = data.castTo(newTree(nnkPtrTy, ty))
    echo treerepr ty
    let obj_ty = ty.getType
    echo treerepr obj_ty
    for field in obj_ty[1].children:
      let field_ty = getType(field)
      let ftyk = field_ty.typekind
      if ftyk in {ntyString, ntyRef, ntySequence}:
        echo field
        result.add(quote do:
          `data`.`field` = nil)
      else:
        echo ftyk


  of ntyRef, ntySequence:
    let data = data.castTo(newTree(nnkPtrTy, ty[0]))
    let dsym = genSym(nskLet, "data")
    return quote do:
      let `dsym` = `data`
      if not `dsym`[].isNil: `data`[] = nil#GCunref(`dsym`[])

  of ntyProc:
    # stmts.add(quote do: `data`[] = nil)
    # ty_sym = ty[0]
    let data = data.castTo(newTree(nnkPtrTy, ty[0]))
    return quote do:
      `data`[] = nil

  of ntyDistinct:
    # TODO fix later, remove the need for an extra function call
    echo treerepr ty
    let ty = if ty.kind == nnkSym: getType(ty) else: ty
    let subt = ty[1]
    return genDecrefStmts(subt, data)

  of ntyCstring:
    discard 

  else:
    echo "what do for ", ty.typekind, "???"
    echo treerepr ty
    echo treerepr(getType(ty))
    quit 0



proc destroyComponent* [t: any] (self:Object; data:pointer) {.nimcall.} =
  # todo gcunref all non-nil refs (provide a default destructor)
  macro decRefs : stmt =
    let x: NimNode = (quote do: t)[0]
    echo x.treerepr
    var ty = getType(x)
    var tyk = ty.typekind
    if tyk == ntyTypedesc:
      ty = ty[1]
      tyk = ty.typekind

    var ty_sym = ty

    var stmts = genDecrefStmts(ty_sym, ident"data")
    result = stmts

    # if skipCast:
    #   result = stmts
    # else:
    #   let pt = newTree(nnkPtrTy, ty_sym)
    #   echo repr pt
    #   result = newStmtList(quote do: 
    #     let `data` = cast[`pt`](`data`))
    #   result.add stmts

    when defined(debug):
      echo "destroyComponent macro result: " , repr result

  when defined(debug):
    echo "destroyComponent invoked ", name(t)
  static:
    echo name( t)
  decRefs()

proc next_type_id (t:typedesc): NimDataTypeID =
  #mixin destroyComponent
  result = data_type_counter.NimDataTypeID
  var c = Component(
    name: name(t), 
    bytes: sizeof(t),
    messages: initTable[string,Object](4),
    kind: ComponentKind.Static,
    nimType: result,
    destructor: ObjectDestructor(destroyComponent[t])
  )
  if staticComponents.len < c.nimType+1:
    staticComponents.setLen c.nimType+1
  staticComponents[c.nimType] = c

  data_type_counter += 1

proc data_type_id* (t:typedesc): NimDataTypeID =
  ## accessor used to check if a component is a data type
  # static: echo name(t)
  # echo name(t)
  var id{.global.} = next_type_id(t)
  return id

#proc gt*(n: typedesc): NimNode {.magic: "NGetType", noSideEffect.}
#template getType* (t:typedesc): NimNode = getType(t)


proc dataComponent* (i:int): Component =
  staticComponents[i]

proc typeComponent* (t:typedesc): Component =
  dataComponent(dataTypeID(t))



proc `$` (co: Component): string =
  "(Component $#:$#)".format(
    if co.nimType == 0.NimDataTypeID: "0x"& $toHex(cast[int](co), sizeof(int)*2) else: $co.nimType,
    co.name
  )



## aggregate

proc aggregate* (cos: varargs[Component]): AggregateType =
  # create a new type out of multiple components

  result = AggregateType(components: newSeq[(int,Component)](cos.len))
  var bytes = 0
  for i in 0 .. high(cos):
    let c = cos[high(cos)-i]
    result.components[i] = (bytes,c)
    bytes += c.bytes
  result.bytes = bytes

proc rfind* (a: seq, b: any): int {.inline.}=
  result = high(a)
  while result > low(a):
    if a[result] == b: return
    dec result

proc findComponentIndex* (ty: AggregateType; co: Component): int =
  proc `==` (a: (int,Component), b: Component): bool =
    a[1] == b
  ty.components.rfind(co)


proc findComponentOffset* (ty: AggregateType; co: Component): int =
  result = ty.findComponentIndex co
  if result == -1: return
  result = ty.components[result][0]



## Object



var 
  aggx_undef*: AggregateType # global aggregate type used for nil
    # todo add some safety that no components on here have data. 'twould be bad

template safeType* (obj: Object): AggregateType =
  (if obj.isNil: aggx_undef else: obj.ty)


proc printComponentNames* (ty: AggregateType; sep = ", "): string =
  result = ""
  for i in countdown(ty.components.high, 0, 1):
    result.add ty.components[i][1].name
    if i > 0:
      result.add sep

proc simpleRepr* (obj: Object): string =
  result = "("
  result.add obj.safeType.printComponentNames()
  result.add ')'

when defined(Debug):
  var objBeingFreed*: proc(o:Object)
proc freeObject* (obj: Object) =
  #obj.sendMessage("beingFreed")
  echo "Object free'd: 0x",strutils.tohex(cast[int](obj), sizeof(pointer)*2)
  echo "  (", printComponentNames(obj.ty), ")"
  when defined(Debug):
    if not objBeingFreed.isNil:
      objBeingFreed(obj)
  for ofs,component in obj.ty.components.items:
    case component.kind
    of ComponentKind.Static:
      if not component.destructor.isNil:
        let component_data = obj.dat[ofs].addr
        component.destructor obj, component_data

    of ComponentKind.Dynamic:
      let my_data = cast[ptr UncheckedArray[Object]](obj.dat[ofs].addr)
      for idx in 0 .. high(component.slots):
        template slot: Object = my_data[idx]
        if not slot.isNil:
          echo "slot ", component.slots[idx], " ref count: ", slot.getRefCount
          #GCunref slot
          slot = nil

  obj.ty = nil

when true or defined(nim_fix_unsaferef_free):
  proc x = 
    var e: Object
    new e, freeObject
    e.ty = AggregateType(components: @[])
  block: x()
  GC_fullcollect()



proc instantiate* (ty: AggregateType): Object =
  unsafeNew result, ty.bytes + sizeof(AggregateType)
  result.ty = ty
  # todo run initializers


proc dataPtr* (obj:Object; ty:typedesc): ptr ty =
  let idx = obj.safeType.findComponentOffset(typeComponent(ty))
  if idx == -1: return
  return cast[ptr ty](obj.dat[idx].addr)
proc dataVar* (obj:Object; ty:typedesc): var ty =
  obj.findData(ty)[]

proc findData* (obj:Object; ty:typedesc): ptr ty =
  dataPtr(obj,ty)
proc findDataM* (obj:Object; ty:typedesc): var ty =
  dataVar(obj,ty)



template printComponents* (e:Object): stmt =
  let ty = e.safeType
  for offs,c in items(ty.components):
    echo "  ", c.name
    for k in keys(c.messages):
      stdout.write k
      stdout.write ", "
    stdout.write '\L'
    stdout.flushfile
  echo ty.components.len , " total"


# proc send* (co:Component; msg:string; args:varargs[Object]): Object =
#   let m = co.messages[msg]
#   if not m.isNil:
#     var bm: BoundComponent
#     bm.comp = co
#     bm.offs = -1
#     result = m(bm, args)




## dynamic components

# proc readSlot (idx:int): MessageImpl =
#   return proc(this:BoundComponent; args:varargs[Object]):Object =
#     let offs = this.offs + (idx * sizeof(pointer))
#     return cast[ptr Object](this.self.dat[offs].addr)[]
# proc writeSlot (idx:int): MessageImpl =
#   return proc(this:BoundComponent; args:varargs[Object]):Object =
#     let offs = this.offs + (idx * sizeof(pointer))
#     cast[var Object](this.self.dat[offs].addr) = args[0]

# proc dynaComponent* (name: string; slots: varargs[string]): Component =
#   result = Component(
#     bytes: slots.len * sizeof(pointer),
#     name: name,
#     messages: initTable[string,MessageImpl](),
#     kind: ComponentKind.Dynamic,
#     slots: @slots
#   )
#   for i in 0 .. high(slots):
#     let 
#       m_reader = slots[i]
#       m_writer = m_reader&":"
#     result.rawDefine m_reader, readSlot(i)
#     result.rawDefine m_writer, writeSlot(i)








## aggregate behavior modifying


proc dup* (ty:AggregateType): AggregateType =
  AggregateType(bytes: ty.bytes, components: ty.components)

proc isBehavior* (co:Component): bool = co.bytes == 0
  ## component stores no state

proc addBehavior* (ty:AggregateType; behav:Component): bool =
  ## modify an aggregate type by adding behavior, all instances of 
  ## the type will be affected!
  result = behav.isBehavior
  if not result: return false

  ty.components.add((0,behav))

proc addBehavior* (obj:Object; behav:Component): bool =
  ## derive a new aggregate type for obj with behav added. 
  ## no other instances of the current type will be affected.
  result = behav.isBehavior
  if not result: return false

  let new_ty = obj.ty.dup
  new_ty.components.add((-1,behav))
  obj.ty = new_ty

proc dropBehavior* (ty:AggregateType; behav:Component): bool =  
  ## modifies an aggregate type, all instances will be affected!
  result = behav.isBehavior
  if not result: return false

  let idx = ty.findComponentIndex(behav)
  if idx == -1: return false

  ty.components.delete idx

proc dropBehavior* (obj:Object; behav:Component): bool =
  ## derive a new type for the object without `behav`. this does not
  ## modify other objects, instead the object is assigned a new copy of type.
  result = behav.isBehavior
  if not result: return false

  let idx = obj.ty.findComponentIndex(behav)
  if idx == -1: return false

  let new_ty = obj.ty.dup
  new_ty.components.delete idx
  obj.ty = new_ty

proc insertBehavior* (ty:AggregateType; behavior:Component; index:Natural): bool=
  ## insert a behavior into a specific slot
  ## index should range from 0 .. high(ty.components)
  ## index 0 inserts at the end of the components since dispatch looks in reverse
  result = behavior.isBehavior
  if not result: return
  
  ty.components.insert(
    (-1,behavior),
    ty.components.len - index)





