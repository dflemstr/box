package org.openpandora.box.model

import org.squeryl.Schema
import net.liftweb.common.Logger
import net.liftweb.util.LoanWrapper
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session

object Database extends Schema
                   with Logger {
  val appMetas         = table[AppMeta]        ("appmetas")
  val applications     = table[Application]    ("applications")
  val categories       = table[Category]       ("categories")
  val comments         = table[Comment]        ("comments")
  val packages         = table[Package]        ("packages")
  val packageDownloads = table[PackageDownload]("packageDownloads")
  val ratings          = table[Rating]         ("ratings")
  val users            = table[User]           ("users")
  val searchKeywords   = table[SearchKeyword]  ("searchKeywords")

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

  applicationsToAppMetas        .foreingKeyDeclaration.constrainReference(onDelete cascade)
  applicationsToCategories      .foreingKeyDeclaration.constrainReference(onDelete cascade)
  applicationsToAppMetas        .foreingKeyDeclaration.constrainReference(onDelete cascade)
  applicationsToCategories      .foreingKeyDeclaration.constrainReference(onDelete cascade)
  applicationsToComments        .foreingKeyDeclaration.constrainReference(onDelete cascade)
  applicationsToRatings         .foreingKeyDeclaration.constrainReference(onDelete cascade)

  packagesToApplications        .foreingKeyDeclaration.constrainReference(onDelete cascade)
  packagesToPackageDownloads    .foreingKeyDeclaration.constrainReference(onDelete cascade)

  usersToComments               .foreingKeyDeclaration.constrainReference(onDelete cascade)
  usersToPackageDownloads       .foreingKeyDeclaration.constrainReference(onDelete cascade)
  usersToPackages               .foreingKeyDeclaration.constrainReference(onDelete cascade)
  usersToRatings                .foreingKeyDeclaration.constrainReference(onDelete cascade)

  def buildLoanWrapper(): LoanWrapper = new LoanWrapper {
    def apply[A](f: => A): A = transaction{
      f
    }
  }
}
