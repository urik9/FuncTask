

/**
 * Created by Uri_Keinan on 8/5/14.
 */
class FuncDataTransform {

  def count(lri: List[LoggedItem]) = lri.size

  def responseSize(loggedResponses: List[LoggedResponse]):Int = {
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

  def sortByMostPopularUrls(requests: List[LoggedRequest]) = {
    requests.groupBy(request => request.url).toArray.sortBy(request => count(request._2)).reverse
  }
  def getTwoMostPopularUrls(requests: List[LoggedRequest]) = {
    val twoMost = sortByMostPopularUrls(requests)
    (twoMost(0)._1, twoMost(1)._1)
  }

  def getInfoForURL(requests: List[LoggedRequest]) = {
    val urlToInfo = sortByMostPopularUrls(requests)
    (urlToInfo(0)._1,count(urlToInfo(0)._2),
      errorRate(urlToInfo(0)._2),
      requestToResponseSize(urlToInfo(0)._2))
  }


  def requestToResponseSize(requests: List[LoggedRequest]) = {
    for (request <- requests)
      (request.httpType, request.status) match {
        case ("GET", "200") => new LoggedResponse("_", 200)
        case ("POST", "200") => new LoggedResponse("_", 100)
        case (_, _) => List.empty
          .toList
      }
  }


}