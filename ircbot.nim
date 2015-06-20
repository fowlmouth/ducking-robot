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


defineMessage(cxIRCclient, "onConnect")
do: return

defineMessage(cxIRCclient, "onDisconnect")
do: return

defineMessage(cxIRCclient, "onPrivMsg")
do(msg): 
  echo msg.asString[]

defineMessage(cxIRCclient, "poll") do:
  let client = this.dataPtr(IRCclient).client
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
        handler = "onPrivMsg"
      of MNotice:
        echo event.params
      else:
        echo "missed event ", event.cmd
        echo event.repr
        return
    else:
      echo event
      return

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
  let arr = args.asArray
  if arr.isNil: return
  let newCtx = createMethodCallContext(
    context, self, msg.asString[], arr.elems)
  context.dataPtr(Context).exec.setActiveContext newCtx


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



# proc onIrcEvent(client: PAsyncIrc, event: TIrcEvent) {.async.} =
#   case event.typ
#   of EvConnected:
#     nil
#   of EvDisconnected, EvTimeout:
#     await client.reconnect()
#   of EvMsg:
#     if event.cmd == MPrivMsg:
#       var msg = event.params[event.params.high]
#       const prefix = "!ft "

#       if msg == "!part":
#         await client.part(event.origin, "</3")
#         return

#       if event.origin in ignores:
#         return
      
#       if msg.continuesWith(prefix, 0):
#         let code = msg[len(prefix) .. ^1]
#         let o = execute(code)
#         if o.isSome:
#           let str = o.unsafeGet.send("print").asString
#           if not str.isNil:
#             await client.privmsg(event.origin, str[])
#       if msg.continuesWith("!join ",0):
#         let chan = msg["!join ".len .. ^1]
#         if chan.len == 0 or chan[0] != '#':
#           return
#         echo "joining ", chan.repr
#         await client.join(chan)
#       if msg == "!stfu":
#         ignores.add event.origin
#       if msg == "!list-ignores":
#         await client.privmsg(event.origin, ignores.join(","))
#       if msg == "!clear-ignores":
#         ignores.setLen 0
#       if msg == "!test":
#         await client.privmsg(event.origin, "hello")
#       if msg == "!lag":
#         await client.privmsg(event.origin, formatFloat(client.getLag))
#       # if msg == "!excessFlood":
#       #   for i in 0..10:
#       #     await client.privmsg(event.origin, "TEST" & $i)
#       # if msg == "!users":
#       #   await client.privmsg(event.origin, "Users: " &
#       #       client.getUserList(event.origin).join("A-A"))
#     echo(event.raw)


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
    self joinChannel: '#fancyfeast'
  ].

  bot connect.  
  Lobby at: 'botfowl' put: bot.

  bot
] value

"""


  let s = execute """
[i bot|
  i: 0.
  bot: Lobby botfowl.
  [i < 800000000] whileTrue: [
    bot poll.
    i: i+1
  ].
  i
] value
"""
  echo s.unsafeGet.simpleRepr
