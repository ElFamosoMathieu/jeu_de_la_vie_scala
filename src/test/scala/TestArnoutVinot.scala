import org.scalatest._
import ArnoutVinot._

class TestArnoutVinot extends FunSuite{

  //donnees test
  val l = List(" XX","  X","XXX")
  val l2 = List("XXX","XXX","XXX")
  val l3 = List("XXXXX")
  val l4 = List("X","X","X","X","X")
  val l5 = List("                X")




    /**
   * tests pour la conversion de chaines en grille
   */
  test("grille de la question 1 est bien convertie"){
    val grilletest:Grille = List((0,1), (0,2), (1,2), (2,0), (2,1), (2,2))

    assert(chainesToGrille(l)==grilletest)
  }

  test("grille carre bien convertie"){
    val grilletest:Grille = List((0,0),(0,1), (0,2),  (1,0), (1,1), (1,2), (2,0), (2,1), (2,2))

    assert(chainesToGrille(l2)==grilletest)
  }

  test("grille ligne bien convertie"){
    val grilletest:Grille = List((0,0),(0,1),(0,2),(0,3),(0,4))

    assert(chainesToGrille(l3)==grilletest)
  }

  test("grille colone bien convertie"){
    val grilletest:Grille = List((0,0),(1,0),(2,0),(3,0),(4,0))

    assert(chainesToGrille(l4)==grilletest)
  }

  test("grille avec beaucoup de vide bien convertie"){
    val grilletest:Grille = List((0,16))

    assert(chainesToGrille(l5)==grilletest)
  }


  /**
   * Lorsque le test precedent est execute, ScalaTest indique simplement que le 
   * test a echoue, mais n'explique pas pourquoi. On obtient quelque chose comme cela
   * 
   *
   * {{{
   *    [info] - one plus one is three? *** FAILED ***
   * }}}
   *
   * Pour avoir plus de details, on utilise une egalite speciale, 
   * `===` au lieu de `==` (utilisable uniquement dans ScalaTest). Avec le test suivant, 
   * on obtient
   * 
   * {{{
   *    [info] - details pourquoi un plus plus n'est pas trois *** FAILED ***
   *    [info]   2 did not equal 3 
   * }}}
   *
   * Utilisez donc toujours l'operateur `===` pour ecrire des tests.
   */



  /**
   * Pour tester les exceptions, on utilise l'operation "intercept". Dans le test suivant,
   * on verifie que la methode entierDifferentDeZero (defini dans l'objet objHello) lance
   * l'exception "IllegalArgumentException" si son argument est 0
   *
   * Notez le "import" qui permet d'utiliser les methodes definies dans l'objet objHello
   */


  

  
}