

/**
 * Created by Uri_Keinan on 8/5/14.
 */
class FuncDataTransform {
  def count(lri: List[LoggedItem]) = lri.size

  def responseSize(lrp: List[LoggedResponse]) = {
    var rs = 0
    for (rp <- lrp)
      rs = rs + rp.respSize
    rs
  }

  def errorRate(lrq: List[LoggedRequest]): Double = {
    var err: Double = 0.0
    for (rq <- lrq) {
      if (rq.status != "200")
        err = err + 1
    }
    err / count(lrq)
  }

  def errorRateSpecific(lrq: List[LoggedRequest]) = {
    val errorRateByType: List[Double] = List.empty
    val resultlist = lrq.groupBy(rq => rq.status.charAt(0))
    for (result <- resultlist) {
      errorRateByType :+ errorRate(result._2)
    }
    errorRateByType


  }


}