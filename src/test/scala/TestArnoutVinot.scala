import org.scalatest._

class TestArnoutVinot extends FunSuite{

  import ArnoutVinot._

  test("voisines 8"){
    assert(voisines8(1,1) == (0,0)::(0,1)::(0,2)::(1,0)::(1,2)::(2,0)::(2,1)::(2,2)::Nil)
  }

  test("pas de survivantes"){
    val l = List("X  ", "  ", "  X");
    val grille = survivantes(chainesToGrille(l))
    val grille_témoin = List()

    assert(grille == grille_témoin)
  }
}