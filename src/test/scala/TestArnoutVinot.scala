
import org.scalatest._

class TestArnoutVinot extends FunSuite{
  
    /**
   * Les tests unitaires en Scala sont ecrits avec l'operateur "test", qui prend 2 arguments : 
   *
   * - Une description (une chaine), pour identifier le test. Cette description doit etre unique, 
   *   deux tests ne peuvent pas avoir la meme description  
   * - Le corps du test, du code Scala qui implemente le test 
   * 
   * La maniere habituelle d'ecrire un test est d'utiliser la methode "assert" 
   * qui teste si son argument s'evalue à vrai. Voici un example simple de test reussi 
   */
  test("un plus un égal deux"){
    assert(1 + 1 == 2)
  }

  /**
   * Voici un example de test qui echoue
   */
  test("un plus un égal trois ?") {
    assert(1 + 1 == 3) //echec ! Testez puis corrigez cette expression pour que le test passe 
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
  test("details pourquoi un plus un n'est pas trois") {
    assert(1 + 1 === 3) // Testez puis corrigez l'expression
  }


  /**
   * Pour tester les exceptions, on utilise l'operation "intercept". Dans le test suivant, 
   * on verifie que la methode entierDifferentDeZero (defini dans l'objet objHello) lance
   * l'exception "IllegalArgumentException" si son argument est 0
   * 
   * Notez le "import" qui permet d'utiliser les methodes definies dans l'objet objHello
   */
  import objHello._
  
  test("entierDifferentDeZero lance une exception avec 0") {
    intercept[IllegalArgumentException] {
      entierDifferentDeZero(0)
    }
  }
  

  
}