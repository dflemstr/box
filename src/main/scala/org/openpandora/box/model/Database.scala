package org.openpandora.box.model

import org.squeryl.Schema
import net.liftweb.util.DynoVar
import net.liftweb.util.LoanWrapper
import org.squeryl.PrimitiveTypeMode._

object Database extends Schema {
  val appMetas         = table[AppMeta]        ("appmetas")
  val applications     = table[Application]    ("applications")
  val categories       = table[Category]       ("categories")
  val comments         = table[Comment]        ("comments")
  val packages         = table[Package]        ("packages")
  val packageDownloads = table[PackageDownload]("packageDownloads")
  val ratings          = table[Rating]         ("ratings")
  val users            = table[User]           ("users")

  val applicationsToAppMetas     = oneToManyRelation(applications, appMetas)        .via((a, am) => a.id === am.applicationId)
  val applicationsToCategories   = oneToManyRelation(applications, categories)      .via((a, c)  => a.id === c .applicationId)
  val applicationsToComments     = oneToManyRelation(applications, comments)        .via((a, c)  => a.id === c .applicationId)
  val applicationsToRatings      = oneToManyRelation(applications, ratings)         .via((a, r)  => a.id === r .applicationId)
  
  val packagesToApplications     = oneToManyRelation(packages,     applications)    .via((p, a)  => p.id === a .packageId)
  val packagesToPackageDownloads = oneToManyRelation(packages,     packageDownloads).via((p, pd) => p.id === pd.packageId)

  val usersToComments            = oneToManyRelation(users,        comments)        .via((u, c)  => u.id === c .userId)
  val usersToPackageDownloads    = oneToManyRelation(users,        packageDownloads).via((u, pd) => u.id === pd.userId)
  val usersToPackages            = oneToManyRelation(users,        packages)        .via((u, p)  => u.id === p .userId)
  val usersToRatings             = oneToManyRelation(users,        ratings)         .via((u, r)  => u.id === r .userId)

  def buildLoanWrapper(): LoanWrapper = new LoanWrapper {
    def apply[A](f: => A): A = transaction(f)
  }
}
