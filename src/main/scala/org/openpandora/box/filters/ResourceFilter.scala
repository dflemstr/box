package org.openpandora.box.filters

import javax.servlet._
import javax.servlet.http._
import net.liftweb.util.Helpers

class ResourceFilter extends Filter {
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) {
    (req, res) match {
      case (httpReq: HttpServletRequest, httpRes: HttpServletResponse) =>
        val url = httpReq.getRequestURI
        if(!httpRes.containsHeader("Expires")) {
          httpRes.addHeader("Expires", Helpers.toInternetDate((new java.util.Date).getTime + 3l*24*60*60*1000))
        }

        if(url endsWith ".eot")
          httpRes.setContentType("application/vnd.ms-fontobject")
        else if(url endsWith ".ttf")
          httpRes.setContentType("application/x-font-ttf")
        else if(url endsWith ".woff")
          httpRes.setContentType("application/x-font-woff")
      case _ =>
    }
    chain.doFilter(req, res)
  }
  def init(config: FilterConfig) = ()
  def destroy() = ()
}
