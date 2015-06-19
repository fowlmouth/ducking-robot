import irc, asyncdispatch, strutils
import vm, stdobj, cmodel, options, tests.vmtest11

var ignores: seq[string] = @[]

proc onIrcEvent(client: PAsyncIrc, event: TIrcEvent) {.async.} =
  case event.typ
  of EvConnected:
    nil
  of EvDisconnected, EvTimeout:
    await client.reconnect()
  of EvMsg:
    if event.cmd == MPrivMsg:
      var msg = event.params[event.params.high]
      const prefix = "!ft "

      if msg == "!part":
        await client.part(event.origin, "</3")
        return

      if event.origin in ignores:
        return
      
      if msg.continuesWith(prefix, 0):
        let code = msg[len(prefix) .. ^1]
        let o = execute(code)
        if o.isSome:
          let str = o.unsafeGet.send("print").asString
          if not str.isNil:
            await client.privmsg(event.origin, str[])
      if msg.continuesWith("!join ",0):
        let chan = msg["!join ".len .. ^1]
        if chan.len == 0 or chan[0] != '#':
          return
        echo "joining ", chan.repr
        await client.join(chan)
      if msg == "!stfu":
        ignores.add event.origin
      if msg == "!list-ignores":
        await client.privmsg(event.origin, ignores.join(","))
      if msg == "!clear-ignores":
        ignores.setLen 0
      if msg == "!test":
        await client.privmsg(event.origin, "hello")
      if msg == "!lag":
        await client.privmsg(event.origin, formatFloat(client.getLag))
      # if msg == "!excessFlood":
      #   for i in 0..10:
      #     await client.privmsg(event.origin, "TEST" & $i)
      # if msg == "!users":
      #   await client.privmsg(event.origin, "Users: " &
      #       client.getUserList(event.origin).join("A-A"))
    echo(event.raw)

var client = newAsyncIrc(
  "irc.freenode.net", 
  nick="botfowl",
  joinChans = @["#fowlsoft"], 
  callback = onIrcEvent)
asyncCheck client.run()

runForever()