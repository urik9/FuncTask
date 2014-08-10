import scala.collection.immutable.Nil

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

  //    val errorRateByType: List[Double] = List.empty
  //    val resultlist = lrq.groupBy(rq => rq.status.charAt(0))
  //    for (result <- resultlist) {
  //     println(count(result._2))
  //     errorRateByType :+ (count(result._2))
  //    }
  //    errorRateByType

}