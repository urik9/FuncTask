/**
 * Created by Uri_Keinan on 8/5/14.
 */
class DataTransform {
  def count(rq :List[LoggedRequest]) = {
    if (rq == List.empty)
      "No Requests"
    else rq.size + " Requests Sent"
  }
  
 def responseSize(rq: List[LoggedRequest]) = {
   var i = 0
   for(srq <- rq){
     if(srq == GetRequestSuccessful)
        i = i+200
   }
   i+"k response"
 }
  def errorRate(rq: List[LoggedRequest]) = {
    var e: Double = 0
    for(srq <- rq){
      if(srq != GetRequestSuccessful)
        e = e+1
    }
    "error rate is "+(e/(rq.size))
  }
}
