import Oraculo._
import ReconstCadenas._
import ReconstCadenas.reconstruirCadenaTurbo
import scala.util.Random

val random = new Random()

def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
 //Crea una secuencia de long caracteres del alfabeto,
//  // escogidos de forma aleatoria, terminando en s
if (s.length==long) s
else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long,alfabeto(indiceAzar)+:s)
  }
}
val costoOraculo = 1

val sec1=Seq('a', 'c', 'c', 'a')
val sec2 = Seq('a', 'c', 'g', 'c', 'a')
val sec3=secAlAzar(10,Seq())
val sec4= Seq('a','t')
val sec8= Seq('a','t','g','g','g','a','t','c')
val or_1=crearOraculo(costoOraculo)(sec1)
val or_2=crearOraculo(costoOraculo)(sec2)
val or_3=crearOraculo(costoOraculo)(sec3)
val or_4=crearOraculo(costoOraculo)(sec4)
val or_8=crearOraculo(costoOraculo)(sec8)
//reconstruirCadenaIngenuo(sec1.length, or_1)
//reconstruirCadenaIngenuo(sec2.length, or_2)
//reconstruirCadenaIngenuo(sec3.length, or_3)
reconstruirCadenaTurbo(alfabeto,sec8.length,or_8)
reconstruirCadenaIngenuo(sec8.length, or_8)