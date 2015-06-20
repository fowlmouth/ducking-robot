import irc, asyncdispatch, strutils, times
import vm, stdobj, cmodel, options

type
  IRCclient = object
    client: PIRC
defPrimitiveComponent(IRCclient, IRCclient)
#defPrimitiveComponent(IRCevent,  TIrcEvent)

registerNewStaticComponents()

let cxIRCclientCls = slotsComponent("IRCclient-ClassBehavior")
block:
  let objCxIRCclient = getComponentComponent("IRCclient")
  assert objCxIRCclient.addBehavior cxIRCclientCls


#defineMessage(cxIRCclientCls, "connectTo:nick:")

# client = newIRC(
#   "irc.freenode.net", 
#   nick="botfowl",
#   joinChans = @["#fowlsoft"], 
#   callback = onIrcEvent)

defineMessage(cxIRCclientCls, "address:port:nick:user:realname:")
do (address,port,nick,user,realname):
  result = aggxIRCclient.instantiate
  result.dataVar(IRCclient).client = newIRC(
    address.asString[], 
    port.asInt[].Port,
    nick.asString[], 
    user.asString[],
    realname.asString[]
  )


defineMessage(cxIRCclient, "sendRaw:")
do(msg):
  this.dataPtr(IRCclient).client.send(
    msg.asString[] )

defineMessage(cxIRCclient, "message:text:")
do(target,msg):
  this.dataPtr(IRCclient).client.privmsg(
    target.asString[], msg.asString[] )

defineMessage(cxIRCclient, "notice:text:")
do(target,msg):
  this.dataPtr(IRCclient).client.notice(
    target.asString[], msg.asString[]  )

defineMessage(cxIRCclient, "joinChannel:")
do(chan):
  this.dataPtr(IRCclient).client.join(
    chan.asString[] )

defineMessage(cxIRCclient, "partChannel:reason:")
do(chan,reason):
  this.dataPtr(IRCclient).client.part(
    chan.asString[],
    reason.asString[])
defineMessage(cxIRCclient, "close") do:
  this.dataPtr(IRCclient).client.close
defineMessage(cxIRCclient, "connect") do:
  this.dataPtr(IRCclient).client.connect
defineMessage(cxIRCclient, "reconnect") do:
  this.dataPtr(IRCclient).client.reconnect

defineMessage(cxIRCclient, "isConnected") do:
  if this.dataPtr(IRCclient).client.isConnected: obj_true else: obj_false

defineMessage(cxIRCclient, "onConnect")
do: return

defineMessage(cxIRCclient, "onDisconnect")
do: return

defineMessage(cxIRCclient, "onPrivMsg")
do(msg,origin): 
  echo msg.asString[]

defineMessage(cxIRCclient, "poll") do:
  let client = this.dataPtr(IRCclient).client
  if not client.isConnected:
    return

  var event: TIrcEvent
  if client.poll(event):
    var handler: string
    var args: seq[Object]

    case event.typ
    of EvConnected:
      handler = "onConnect"
    of EvDisconnected:
      handler = "onDisconnect"
    of EvMsg:
      case event.cmd
      of MPrivMsg:
        var msg = event.params[event.params.high]
        args.safeAdd msg.asObject
        args.safeAdd event.origin.asObject
        handler = "onPrivMsg"
      of MNotice:
        echo event.params
      else:
        #echo "missed event ", event.cmd
        #echo event.repr
        discard
    else:
      #echo event
      discard

    if handler.isNil: return

    let newCtx = createMethodCallContext(
      context, self, handler, args)
    context.dataPtr(Context).exec.setActiveContext newCtx

defineMessage(cxObj, "send:")
do(msg):
  let newCtx = createMethodCallContext(
    context, self, msg.asString[], nil )
  context.dataPtr(Context).exec.setActiveContext newCtx

defineMessage(cxObj, "send:withArgs:")
do(msg,args):
  let arr = args.dataPtr(Array)
  if arr.isNil: return
  let newCtx = createMethodCallContext(
    context, self, msg.asString[], arr.elems)
  context.dataPtr(Context).exec.setActiveContext newCtx

defineMessage(cxObj, "addBehavior:") do (co):
  let cp = co.dataPtr(Component)
  assert(not cp.isNil)
  assert(cp[].isBehavior)
  assert(not self.isNil)
  let t = self.ty.mutate(before = [cp[]])
  self.ty = t
  return co

defineMessage(cxContext, "evaluateCode:") do (code):
  let str = code.asString
  if str.isNil: return

  let meth = compileExpressions(str[])
  if meth.isNone: return
  let recv = newAnonObj()
  let ctx = createContext(
    meth.unsafeGet,
    recv.getComponent(0) )
  ctx.dataVar(Context).caller = context
  #ctx.dataVar(Context).exec = context.exec
  context.dataPtr(Context).exec.setActiveContext ctx

template objBool* (something): expr =
  (if something: obj_true else: obj_false)

defineMessage(cxString, "startsWith:") do (str):
  let str2 = str.asString
  if str2.isNil: return obj_false
  result = this.dataVar(string).continuesWith(str2[], 0).objBool

defineMessage(cxString, "from:to:") do (a,b):
  let inta = a.asInt
  let intb = b.asInt
  if inta.isNil or intb.isNil: return "".asObject
  let highest = min(intb[], this.dataVar(string).len)
  return substr(self.dataVar(string), inta[], highest).asObject

defineMessage(cxString, "high") do:
  return (self.dataVar(string).len-1).asObject

# if event.cmd == MPrivMsg:
#   var msg = event.params[event.params.high]
#   const prefix = "!ft "

#   if msg == "!part":
#     await client.part(event.origin, "</3")
#     return

#   if event.origin in ignores:
#     return
  
#   if msg.continuesWith(prefix, 0):
#     let code = msg[len(prefix) .. ^1]
#     let o = execute(code)
#     if o.isSome:
#       let str = o.unsafeGet.send("print").asString
#       if not str.isNil:
#         await client.privmsg(event.origin, str[])
#   if msg.continuesWith("!join ",0):
#     let chan = msg["!join ".len .. ^1]
#     if chan.len == 0 or chan[0] != '#':
#       return
#     echo "joining ", chan.repr
#     await client.join(chan)
#   if msg == "!stfu":
#     ignores.add event.origin
#   if msg == "!list-ignores":
#     await client.privmsg(event.origin, ignores.join(","))
#   if msg == "!clear-ignores":
#     ignores.setLen 0
#   if msg == "!test":
#     await client.privmsg(event.origin, "hello")
#   if msg == "!lag":
#     await client.privmsg(event.origin, formatFloat(client.getLag))
#   # if msg == "!excessFlood":
#   #   for i in 0..10:
#   #     await client.privmsg(event.origin, "TEST" & $i)
#   # if msg == "!users":
#   #   await client.privmsg(event.origin, "Users: " &
#   #       client.getUserList(event.origin).join("A-A"))




when isMainModule:

  assert isSome execute """

[bot behavior|
  bot: (Components IRCclient 
          address: 'irc.freenode.net'
          port: 6667
          nick: 'botfowl'
          user: 'botfowl'
          realname: 'botfowl9000').

  behavior: (Components Component newBehavior: 'fowlbot').
  behavior define: 'onConnect' as: [
    self joinChannel: '#fowlsoft'
  ].
  behavior define: 'onDisconnect' as: [
    self reconnect
  ].
  behavior define: 'onPrivMsg' as: [:msg :origin|
    (msg startsWith: '!ft ')
      ifTrue: [code res|
        code: (msg from: 4 to: msg high).
        res: (thisContext evaluateCode: code).
        self message: origin text: res print.
        ^ nil
      ].
    nil
  ].

  bot addBehavior: behavior.
  Lobby at: 'botfowl' put: bot.

  bot
] value

"""


  let s = execute """
[i bot|
  i: 0.
  bot: Lobby botfowl.
  bot connect.
  bot onConnect.

  [i < 100000] whileTrue: [
    bot poll.
    i: i+1
  ].
  i
] value
"""
  echo s.unsafeGet.send("print").asString[]
