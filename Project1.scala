import akka.actor._

object Project1 extends App{
	
//override def main(args:Array[String]):Unit = {
	var n:Long = args(0).toLong;			
	var k:Int = args(1).toInt
	println("Range = "+n)
	println("objects in Sequence = "+k)
	val system=ActorSystem("FirstSystem");
	val supermaster1 = system.actorOf(Props(new Supermaster(k,n)), name = "supermaster1");
	supermaster1 ! "Start"
//}
}

class Supermaster(k:Int, n:Long) extends Actor {
  var count:Int=0;
def receive = {
case "Start"=>
    println("Started supermaster")
	val system=ActorSystem("SecondSystem");
	
	var remainder:Long = n%8;	
	var End:Long = n-remainder
	var Start:Long = 1;
	var zero:Long=0;
	
	if(n>6){
	var master1 = system.actorOf(Props(new master), name = "master1");
	var master2 = system.actorOf(Props(new master), name = "master2");
	var master3 = system.actorOf(Props(new master), name = "master3");
	var master4 = system.actorOf(Props(new master), name = "master4");
	var master5 = system.actorOf(Props(new master), name = "master5");
	var master6 = system.actorOf(Props(new master), name = "master6");
	var master7 = system.actorOf(Props(new master), name = "master7");
	var master8 = system.actorOf(Props(new master), name = "master8");

	count += 8
	
	var part1:Long = n/8;
	var part2:Long = n/4;
	var part3:Long = 3*(n/8);
	var part4:Long = n/2;
	var part5:Long = 5*(n/8);
	var part6:Long = 3*(n/4);
	var part7:Long = 7*(n/8);
	
	
	master1 ! (Start,part1,k);
	master2 ! (part1+1,part2,k);
	master3 ! (part2+1,part3,k);
	master4 ! (part3+1,part4,k);
	master5 ! (part4+1,part5,k);
	master6 ! (part5+1,part6,k);
	master7 ! (part6+1,part7,k);
	master8 ! (part7+1,End,k);
	}
	
	if(remainder>0 || End == zero)
	  count+=1;
	
	if(remainder!=0 || End==zero){
	  var master9 = system.actorOf(Props(new master), name = "master9");
	  master9 !(End+1,n,k);
	  }
	
case (done:String)=>
  	count -= 1;	
  	if(count == 0){
  	 exit()
	//context.system.shutdown()
  	}
  		
}
}

class master extends Actor{

def receive = {
		case (start:Long,end:Long,k:Int) =>
  
		  for(mstart<-start to end ){
		  
			var sum:BigInt = 0
			
			for(i:Long <-mstart to (mstart+k-1)){
				sum +=(i*i)	
			}
			var sqrt:BigInt = ((Math.sqrt(sum.toDouble) +0.5).asInstanceOf[Long])  
	 
	
			if(sqrt*sqrt == sum){
			println(mstart);
			}
			
		  }
		  sender ! "Done";
		}
}
