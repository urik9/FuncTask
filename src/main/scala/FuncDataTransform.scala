

/**
 * Created by Uri_Keinan on 8/5/14.
 */
class FuncDataTransform {

  def count(lri: List[LoggedItem]) = lri.size

  def responseSize(loggedResponses: List[LoggedResponse]) = {
    loggedResponses.foldLeft(0)((acc, response) => acc + response.respSize)
  }

  def errorRate(loggedRequests: List[LoggedRequest]): Double = {
    loggedRequests.foldLeft(0)((acc, request) => (request.status) match {
      case ("200") => acc
      case (_) => acc + 1
    })
      .toDouble / count(loggedRequests)


  }

  def errorRateSpecific(loggedRequests: List[LoggedRequest]) = {
    loggedRequests.filterNot(request => request.status == "200")
      .groupBy(request => request.status.charAt(0)).mapValues(value => count(value).toDouble / count(loggedRequests))
      .toList.sortBy(res => res._1)
  }

  def getByMostPopularUrls(requests: List[LoggedRequest]) = {
    requests.groupBy(request => request.url).toArray.sortBy(request => count(request._2)).reverse

  }
  def getInfoForURL (requests: List[LoggedRequest]) = {
    val urlToInfo = getByMostPopularUrls(requests)
    errorRate(urlToInfo(0)._2)/count(urlToInfo(0)._2)
      urlToInfo(0)._1




    //    urlToInfo(0)._2 match {
    //      case ("GET",_, "200") => List(LoggedResponse(_, 200))
    //      case ("GET",_,_)=> new LoggedError(_)
    //      case ("POST",_,"200")=> List(LoggedResponse(_, 100))
    //    }

  }

}