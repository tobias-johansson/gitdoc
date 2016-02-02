load.ivy("org.eclipse.mylyn.github" % "org.eclipse.egit.github.core" % "2.1.5")

@

import scala.collection.JavaConversions._

import org.eclipse.egit.github.core._
import org.eclipse.egit.github.core.client.GitHubClient
import org.eclipse.egit.github.core.service.RepositoryService
import org.eclipse.egit.github.core.service.ContentsService
import org.eclipse.egit.github.core.service.MarkdownService

import java.util.Base64

implicit class Apply[A](a: A) {
  def |>[B](f: A => B): B = f(a)
}

sealed trait Content { def src: RepositoryContents }
case class File (path: String, src: RepositoryContents) extends Content
case class Dir  (path: String, children: Seq[Content], src: RepositoryContents) extends Content

val ghclient    = new GitHubClient()
val repoService = new RepositoryService(ghclient)
val contService = new ContentsService(ghclient)
val mdService   = new MarkdownService(ghclient)

ghclient.setOAuth2Token("c8a329326458c3cbc4d89b20534060764eba8b5e")
val spout = new Repo("eija-johansson", "spout")

class Repo(userName: String, repoName: String) {

  val repo = repoService.getRepository(userName, repoName)

  def list(path: String = null): Seq[Content] = {
    def convert(c: RepositoryContents) = c.getType match {
      case RepositoryContents.TYPE_FILE => File(c.getPath, c)
      case RepositoryContents.TYPE_DIR  => Dir(c.getPath, list(c.getPath), c)
    }
    val cont = contService.getContents(repo, path)
    cont map convert
  }

  def files = {
    def unwrap(c: Content): Seq[File] = c match {
      case f: File => Seq(f)
      case d: Dir  => d.children flatMap unwrap
    }
    list() flatMap unwrap
  }

  def read(file: File): Option[String] = {
    def decode(b64: String) =
      b64.replaceAll("""\s""","") |> Base64.getDecoder.decode |> (new String(_))

    contService.getContents(repo, file.path)
      .headOption
      .map (_.getContent)
      .map (decode(_))
  }

  def renderMd(file: File): String =
    read(file)
      .map (renderMd)
      .getOrElse ("")

  def renderMd(text: String): String =
    mdService.getRepositoryHtml(repo, text)
}
