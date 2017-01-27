package io.youi.example

import java.io.File

import io.youi.http._
import io.youi.net.ContentType
import io.youi.server.UndertowServer
import io.youi.server.handler.{CachingManager, HttpHandler}

object ExampleServer extends UndertowServer {
  def main(args: Array[String]): Unit = {
    config.clearListeners().addHttpListener("0.0.0.0")

    handler.matcher(path.exact("/hello.txt")).caching(CachingManager.MaxAge(120L)).resource {
      Content.string("Hello World!", ContentType.`text/plain`)
    }
    handler.matcher(path.exact("/test.txt")).caching(CachingManager.LastModified()).resource {
      new File("src/main/web/test.txt")
    }
    handler.matcher(path.exact("/websocket.html")).caching(CachingManager.LastModified()).resource {
      new File("src/main/web/websocket.html")
    }
    handler.matcher(path.exact("/cookies.html")).wrap(CookiesExample)
    handler.matcher(path.exact("/web-socket-example")).wrap(WebSocketExample)
    handler.matcher(path.exact("/proxy.html")).wrap(ProxyExample)
    handler.matcher(path.exact("/session.html")).wrap(SessionExample)
    handler.matcher(path.exact("/communicator")).wrap(ServerExampleCommunication)
    handler.caching(CachingManager.LastModified()).classLoader("", (path: String) => s"content$path")
    handler.caching(CachingManager.LastModified()).classLoader("app", (path: String) => path.drop(4))

    start()
  }
}