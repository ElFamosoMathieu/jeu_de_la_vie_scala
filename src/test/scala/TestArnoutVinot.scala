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

  test("grille carre bien convertie") {
    val grilletest: Grille = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))

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

  test ("estX"){
    val l = List("XXX", "   ", "   ");
    val grille = chainesToGrille(l)
    val res = estX(grille,0,1)
    assert(estX(grille,0,1))
    assert(estX(grille,0,0))
    assert(estX(grille,0,2))
    assert(!(estX(grille,2,2)))
  }

  test("pas de survivantes"){
    val l = List("X  ", "  ", "  X");
    val grille = survivantes(chainesToGrille(l))
    val grille_témoin = List()
    assert(grille == grille_témoin)
  }

  test ("Compte voisines figure 1"){
    val l = List("XXX", "   ", "   ");
    val grille = chainesToGrille(l)
    val compte1 = compteVoisines8(grille,0,0)
    val compte2 = compteVoisines8(grille,0,1)
    val compte3 = compteVoisines8(grille,0,2)
    val compte4 = compteVoisines8(grille,2,2)
    assert(compte1 == 1)
    assert(compte2 == 2)
    assert(compte3 == 1)
    assert(compte4 == 0)
  }

  test ("Compte voisines figure 2"){
    val l = List("XXXXX","  X  ","  X  ");
    val grille = chainesToGrille(l)
    val compte1 = compteVoisines8(grille,0,0)
    val compte2 = compteVoisines8(grille,0,1)
    val compte3 = compteVoisines8(grille,0,2)
    val compte4 = compteVoisines8(grille,0,3)
    val compte5 = compteVoisines8(grille,0,4)
    val compte6 = compteVoisines8(grille,1,2)
    val compte7 = compteVoisines8(grille,2,2)


    assert(compte1 == 1)
    assert(compte2 == 3)
    assert(compte3 == 3)
    assert(compte4 == 3)
    assert(compte5 == 1)
    assert(compte6 == 4)
    assert(compte7 == 1)
  }


  test("survivantes figure 1"){
    val l = List("   ", "XXX", "   ");
    val grille = survivantes(chainesToGrille(l))
    val grille_temoin = List((1,1))
    assert(grille == grille_temoin)
  }

  test( "survivantes figure 2"){

    val grille = survivantes(chainesToGrille(l2))
    val grille_temoin = List((0,0),(0,2),(2,0),(2,2))
    assert(grille==grille_temoin)

  }

  test("survivantes figure 3"){
    val grille = survivantes(chainesToGrille(l3))
    val grille_temoin = List((0,1), (0,2), (0,3))
    assert(grille == grille_temoin)
  }

  test("candidates"){
    val l = List("   ", "XXX", "   ");
    val grille = candidates(chainesToGrille(l))
    val grille_témoin = List((0,-1), (0,0), (0,1), (1,-1), (2,-1), (2,0), (2,1), (0,2), (2,2), (0,3), (1,3), (2,3))
    assert(grille == grille_témoin)
  }

  test("coin supérieur gauche"){
    val l = List("   ", "XXX", "  X");
    val res = coinSupGauche(chainesToGrille(l))
    val témoin = (1,0)
    assert(res == témoin)
  }

  test("coin inférieur droit"){
    val l = List("   ", "XXX", "  X");
    val res = coinInfDroite(chainesToGrille(l))
    val témoin = (2,2)
    assert(res == témoin)
  }

  test("affichage"){
    val l = List("X  ", "XXX", "  X");
    val grille = chainesToGrille(l)
    afficherGrille(grille)
  }

  test("affichage2"){
    val l = List("   ", "XXX", "   ");
    val grille = chainesToGrille(l)
    afficherGrille(grille)
  }

  test("affichage3"){
    val grille = chainesToGrille(l4)
    afficherGrille(grille)
  }
}