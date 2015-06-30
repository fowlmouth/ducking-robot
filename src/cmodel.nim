import tables, typetraits, strutils
export tables, typetraits

type
  Aggr* = ref object
    instanceSize*: int
    components*: seq[ComponentInstance]

  ComponentInstance* = tuple[offs:int, co:Object]

  UncheckedArray*{.unchecked.}[T] = array[1,T]
  Object* = ref object
    ty*: Aggr
    dat*: UncheckedArray[byte]


# cos
type
  MTable* = object
    entries*: Table[string,Object]
  NimDataType* = object
    nimType*: int
    instanceSize*: int
  Slots* = object
    names*: seq[string]

  Identifier* = distinct string




proc `$`* (some:Object): string =
  "0x$#".format(toHex(cast[int](some), sizeof(int)*2))
proc `$`* (some:Aggr): string =
  "(Aggr $# $#)".format(
    some.instanceSize,
    $some.components
  )



var id {.global.} = 0
proc nextID(): int =
  result = id
  id += 1

proc typeID* (ty:typedesc): int =
  var id{.global.} = nextID()
  return id

proc nimDatTy (ty:typedesc): NimDataType {.inline.} =
  NimDataType(instanceSize: sizeof(ty), nimType: typeID(ty))


template static_component_size: int =
  sizeof(MTable) + sizeof(NimDataType) + sizeof(Identifier)

proc allocObj (bytes:int): Object =
  unsafeNew result, bytes+sizeof(Aggr)

template data (o:Object; offs:int): ptr byte =
  o.dat[offs].addr

var 
  cxMtable* : Object

  cxNimdata* : Object
  cxIdentifier* : Object
  aggxStaticComponent* : Aggr

  cxSlots* : Object
  aggxSlots* : Aggr
proc init ()
cmodel.init()

proc typeComponent* (some:typedesc[MTable]): Object =
  cxMtable
proc typeComponent* (some:typedesc[NimDataType]): Object =
  cxNimdata
proc typeComponent* (some:typedesc[Identifier]): Object =
  cxIdentifier
proc typeComponent* (some:typedesc): Object 

proc safeType* (o:Object): Aggr {.inline.}

proc `==` (ci:ComponentInstance; co:Object): bool = 
  ci.co == co
proc findComponentIndex* (obj,co: Object): int {.inline.}=
  obj.safeType.components.find(co)

proc dataPtr* (some:Object; ty:typedesc): ptr ty {.inline.}=
  let idx = some.findComponentIndex(typeComponent(ty))
  assert idx != -1
  let offs = some.ty.components[idx].offs
  assert offs != -1
  result = cast[ptr ty](some.dat[offs].addr)
proc dataVar* (some:Object; ty:typedesc): var ty {.inline.}=
  some.dataPtr(ty)[]

proc `[]`* (some:Object; ty:typedesc): var ty {.inline.}=
  some.dataPtr(ty)[]
proc `[]=`* (some:Object; ty:typedesc; val:ty) {.inline.}=
  (some[ty]) = val


# block:
#   cxMtable[MTable] = initTable[string,Object]()
#   cxMtable[NimDataType] = nimDatTy(MTable)
#   cxMtable[Identifier] = Identifier("MTable")

#   cxNimdata[MTable] = initTable[string,Object]()
#   cxNimdata[NimDataType] = nimDatTy(NimDataType)
#   cxNimdata[Identifier] = Identifier("NimData")





proc instantiate* (some:Aggr): Object = 
  unsafeNew result, sizeof(Aggr)+some.instanceSize
  result.ty = some

var static_components: seq[Object]
proc typeComponent* (index:int): Object =
  do_assert index in 0 .. high(static_components), "Invalid static component "& $index
  return static_components[index]

proc ensureLen* [T] (some: var seq[T]; L: int) {.inline.}=
  if some.len < L: some.setLen L

proc new_component (some:typedesc): Object =
    let id = typeID(some)
    static_components.ensureLen id+1
    result = static_components[id]
    if result.isNil:

      result = aggxStaticComponent.instantiate
      static_components[id] = result

      result[NimDataType] = nimDatTy(some)
      result[MTable].entries = initTable[string,Object]()
      result[Identifier] = name(some).Identifier

      echo "!! new static component (", typeID(some), ") ", name(some)

proc typeComponent* (some:typedesc): Object =
  #static: echo name(some)
  result = new_component(some)
  #echo nimDatTy(some)

proc hasComponent* (obj,co:Object): bool {.inline.}=
  obj.findComponentIndex(co) > -1

proc isStaticComponent* (co:Object): bool {.inline.} =
  co.ty == aggxStaticComponent or co.hasComponent(cxNimdata)


proc numComponents* (ty:Aggr): int = ty.components.len
proc messages* (component:Object): var MTable = component[MTable]

var aggxUndef*: Aggr
proc safeType* (o:Object): Aggr =
  return if o.isNil: aggxUndef else: o.ty


import future
proc printComponentNames* (ty:Aggr; sep=","): string =
  ty.components
  .map( (ci:ComponentInstance) -> string => ci.co[Identifier].string )
  .join(sep)


proc dataSize* (component:Object): int {.inline.}=
  if component.isStaticComponent: 
    component[NimDataType].instanceSize 
  elif component.hasComponent(typeComponent(Slots)):
    component[Slots].names.len * sizeof(Object)
  else:
    # TODO slots
    0

proc aggregate* (components: varargs[Object]): Aggr =
  result = Aggr(components: @[])
  for t in components:
    var offs = -1
    let sz = t.dataSize
    if sz > 0:
      result.components.add((result.instanceSize, t))
      result.instanceSize += sz
    else:
      result.components.add((-1, t))

proc init = 

  #assert id == 0, "id = "& $id

  cxMTable = allocObj(static_component_size)
  cxNimdata = allocObj(static_component_size)
  cxIdentifier = allocObj(static_component_size)
  aggxStaticComponent = Aggr(
    instanceSize: static_component_size,
    components: @[
      (0, cxMtable),
      (sizeof(MTable), cxNimdata),
      (sizeof(MTable)+sizeof(NimDataType), cxIdentifier)
    ]
  )

  assert static_components.isNil
  static_components.newSeq 4

  #assert id == 0

  assert typeID(MTable) == 1, $typeID(MTable)
  cxMtable.ty = aggxStaticComponent
  assert typeID(NimDataType) == 2
  cxNimdata.ty = aggxStaticComponent
  assert typeID(Identifier) == 3
  cxIdentifier.ty = aggxStaticComponent
  template initStaticComponent (o:Object; t:typedesc; name:string): stmt =
    {.line: instantiationInfo() .}:
      o[MTable].entries = initTable[string,Object]()
      o[NimDataType] = nimDatTy(t)
      o[Identifier] = Identifier(name)
      #echo typeID(t)
      static_components[typeID(t)] = o
  initStaticComponent cxMTable, MTable, "MTable"
  initStaticComponent cxNimdata, NimDataType, "NimData"
  initStaticComponent cxIdentifier, Identifier, "Identifier"

  #assert id == 4, "id = "& $id

  # static_components.safeAdd([cxMtable, cxNimdata, cxIdentifier])

  cxSlots = typeComponent(Slots)
  aggxSlots = Aggr(
    instanceSize: sizeof(Slots)+sizeof(MTable)+sizeof(Identifier),
    components: @[
      (0, cxMTable),
      (sizeof(MTable), cxSlots),
      (sizeof(MTable)+sizeof(Slots), cxIdentifier)
    ]
  )

iterator staticComponentsFrom* (start:int): Object =
  for i in start .. high(static_components):
    yield static_components[i]
proc nextStaticID* : int = id

proc isBehavior* (co:Object): bool =
  co.dataSize == 0
proc addBehavior* (co,behavior:Object): bool =
  result = behavior.isBehavior
  if not result: return
  co.ty.components.insert((-1,behavior),0)

proc insertBehavior* (ty:Aggr; beh:Object; idx:int): bool =
  result = beh.isBehavior
  if not result: return
  ty.components.insert((-1,beh), idx)

template `[]`* (some:MTable; key:string): Object = 
  some.entries[key]
template `[]=`* (some:MTable; key:string; val:Object) = 
  some.entries[key] = val


proc printComponents* (some:Object) =
  let ty = some.safeType
  for offs,c in ty.components.items:
    stdout.write ' ', c[Identifier].string, ": "
    for k in c[MTable].entries.keys:
      stdout.write k, ", "
    stdout.write "\n"
    stdout.flushFile

iterator eachComponent* (some:Aggr): Object = 
  for i in 0 .. high(some.components):
    yield some.components[i].co

proc mutate* (some:Aggr; before,after,drop:openarray[Object] = []): Aggr =
  var comps = newSeq[Object]()
  for c in before:
    comps.add c
  for c in some.eachComponent:
    if c notin drop:
      comps.add c
  for c in after:
    comps.add c
  return aggregate comps
