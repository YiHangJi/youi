package spec

import io.youi.http.{HttpConnection, HttpRequest, Method, HttpStatus}
import io.youi.server.dsl._
import io.youi.net._
import io.youi.server.{DefaultErrorHandler, Server}
import org.scalatest.{Matchers, WordSpec}

class ServerDSLSpec extends WordSpec with Matchers {
  private lazy val text = "Hello, World!".withContentType(ContentType.`text/plain`)
  private lazy val html = <html>
    <head>
      <title>Hello, World!</title>
    </head>
    <body>
      <h1>Hello, World!</h1>
    </body>
  </html>.withContentType(ContentType.`text/html`)

  "Server DSL" when {
    "creating a simple handler" should {
      "properly accept a request for /hello/world.txt" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/world.txt")
        val connection = new HttpConnection(server, request)
        server.handle(connection)
        val response = connection.response
        response.content should be(Some(text))
        response.status should be(HttpStatus.OK)
      }
      "properly accept a request for /hello/world.html" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/world.html")
        val connection = new HttpConnection(server, request)
        server.handle(connection)
        val response = connection.response
        response.content should be(Some(html))
        response.status should be(HttpStatus.OK)
      }
      "reject a request from a different origin IP" in {
        val request = HttpRequest(source = ip"127.0.0.2", url = url"http://www.example.com/hello/world.html")
        val connection = new HttpConnection(server, request)
        server.handle(connection)
        val response = connection.response
        response.content should be(Some(DefaultErrorHandler.html(HttpStatus.NotFound)))
        response.status should be(HttpStatus.NotFound)
      }
    }
  }

  object server extends Server {
    handler(
      allow(ip"127.0.0.1", ip"192.168.1.1") :> List(
        Method.Get :> List(
          path"/hello/world.txt" :> respond(text),
          path"/hello/world.html" :> respond(html)
        )
      )
    )
  }
}