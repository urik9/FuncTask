import java.util

/**
 * Created by Uri_Keinan on 8/5/14.
 */
class FuncDataTransform {
  def count (lri: List[LoggedItem]) = lri.size

  def responseSize(lrp: List[LoggedResponse]) = {
    var rs =0
    for(rp <- lrp)
      rs = rs+ rp.respSize
    rs
  }

  def errorRate(lrq: List[LoggedRequest]): Double  = {
    var err: Double = 0.0
    for(rq <- lrq) {
      if (rq.status != "200")
        err = err + 1
    }
        err/ count(lrq)
      }

}