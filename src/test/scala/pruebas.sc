import Oraculo._
import ReconstCadenas._
import ReconstCadenasPar._
import scala.util.Random
import Benchmark._
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
//reconstruirCadenaTurbo(alfabeto,sec8.length,or_8)
//reconstruirCadenaIngenuo(sec8.length, or_8)



def secsCortasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(i,Seq())
} yield s

def secsLargasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(math.pow(2,i).toInt,Seq())
} yield s

val ss1_10=secsCortasParaPruebas(10)
val ss2_1024 = secsLargasParaPruebas(10)


val s_long_2 = ss2_1024(0)
val s_long_4 = ss2_1024(1)
val s_long_8 = ss2_1024(2)
val s_long_16 = ss2_1024(3)
val s_long_32= ss2_1024(4)
val s_long_64 = ss2_1024(5)
val s_long_128 = ss2_1024(6)
val s_long_256 = ss2_1024(7)
val s_long_512 = ss2_1024(8)
val s_long_1024 = ss2_1024(9)

val s_corta_1 = ss1_10(0)
val s_corta_2 = ss1_10(1)
val s_corta_3 = ss1_10(2)
val s_corta_4 = ss1_10(3)
val s_corta_5 = ss1_10(4)
val s_corta_6 = ss1_10(5)
val s_corta_7 = ss1_10(6)
val s_corta_8 = ss1_10(7)
val s_corta_9 = ss1_10(8)
val s_corta_10 = ss1_10(9)

/*
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_2.length,crearOraculo(1)(s_long_2))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_4.length,crearOraculo(1)(s_long_4))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_8.length,crearOraculo(1)(s_long_8))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_16.length,crearOraculo(1)(s_long_16))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_32.length,crearOraculo(1)(s_long_32))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_64.length,crearOraculo(1)(s_long_64))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_128.length,crearOraculo(1)(s_long_128))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_256.length,crearOraculo(1)(s_long_256))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_512.length,crearOraculo(1)(s_long_512))
compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(8))(s_long_1024.length,crearOraculo(1)(s_long_1024))

compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_1.length,crearOraculo(1)(s_corta_1))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_2.length,crearOraculo(1)(s_corta_2))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_3.length,crearOraculo(1)(s_corta_3))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_4.length,crearOraculo(1)(s_corta_4))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_5.length,crearOraculo(1)(s_corta_5))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_6.length,crearOraculo(1)(s_corta_6))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_7.length,crearOraculo(1)(s_corta_7))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_8.length,crearOraculo(1)(s_corta_8))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_9.length,crearOraculo(1)(s_corta_9))
compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(4))(s_corta_10.length,crearOraculo(1)(s_corta_10))

def pruebasLongNTurbo(numPruebas:Int,n:Int,umbral:Int) = for {
  i <- 1 to numPruebas
  s = secAlAzar(math.pow(2,n).toInt,Seq())
} yield compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(umbral))(s.length,crearOraculo(1)(s))
// Long 2
//pruebasLongNTurbo(5,1,1)
// Long 4
//pruebasLongNTurbo(5,2,1)
// Long 8
//pruebasLongNTurbo(5,3,2)
// Long 16
//pruebasLongNTurbo(5,4,4)
// Long 32
pruebasLongNTurbo(3,5,4)
// Long 64
pruebasLongNTurbo(3,6,4)
// Long 128
pruebasLongNTurbo(3,7,4)
// Long 256
pruebasLongNTurbo(3,8,4)
*/
def pruebasLongNIngenuo(numPruebas:Int,n:Int,umbral:Int) = for {
  i <- 1 to numPruebas
  s = secAlAzar(n,Seq())
} yield compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(umbral))(s.length,crearOraculo(1)(s))
/*.
// Long 1
pruebasLongNIngenuo(5,1,1)
// Long 2
pruebasLongNIngenuo(5,2,1)
// Long 3
pruebasLongNIngenuo(5,3,2)
// Long 4
pruebasLongNIngenuo(5,4,2)
// Long 5
pruebasLongNIngenuo(3,5,2)

// Long 6
pruebasLongNIngenuo(3,6,2)
// Long 7
*/

//pruebasLongNIngenuo(3,7,2)
// Long 8
pruebasLongNIngenuo(1,8,2)
// Long 8
pruebasLongNIngenuo(1,9,2)
// Long 10
pruebasLongNIngenuo(1,10,2)

pruebasLongNIngenuo(1,11,2)

pruebasLongNIngenuo(1,12,4)

// val res0: IndexedSeq[(Double, Double, Double)] = Vector((25682.4169,25500.6929,1.007126237734505), (25708.8041,25680.4362,1.001104650239547), (25529.4216,26187.7497,0.9748612191753154)) LONG 7







