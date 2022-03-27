object ArnoutVinot {
  type Grille = List[(Int,Int)]

  //Exercice 1


  /**
   * fonction qui permet de transormer une liste de String en une grille de jeu de la vie
   * @param l liste de String
   * @return grille du jeu de la vie correspondante
   */
  def chainesToGrille(l:List[String]): Grille= l match{
    case Nil => Nil //fin
    case l => aux_chTogrille(l,0)
  }

  /**
   * fonction auxiliaire qui permet de parcourir chaque "ligne" du jeu de la vie avec la fonction stringToGrille
   * @param l liste de String
   * @param ite iterateur qui permet de parcourir toute la liste
   * @return grille du jeu de la vie correspondante
   */
  def aux_chTogrille (l:List[String],ite:Int):Grille= l match {
    case Nil => Nil
    case l => { if (ite == l.length-1) {
      stringToGrille(l(ite),ite,0)
    }else{
      stringToGrille(l(ite),ite,0) ++ aux_chTogrille( l, ite+1)
    }
    }

  }

  /**
   * permet de noter chaque emplacement des elements du jeu de la vie
   * @param chaine string qui corresponds a ne ligne du eju de la vie
   * @param ligne ligne du jeu de la vie
   * @param colonne colonne du jeu de la vie
   * @return un couple de Int correspondant a l'emplacement d'un bloc
   */
  def stringToGrille(chaine : String, ligne : Int, colonne :Int):Grille={
    if (colonne >= chaine.length){Nil

    } else {
      if(chaine.charAt(colonne) == 'X') {
        (ligne,colonne)::stringToGrille(chaine, ligne, colonne+1 )
      } else {
        stringToGrille(chaine, ligne, colonne+1)
      }
    }
  }




  //Exercice 2
  def minX(g:Grille,min:Int):Int =g match {
    case Nil => min
    case (x,y)::q => if (x<min) minX(q,x) else minX(q,min)
  }

  def minY(g:Grille,min:Int):Int =g match {
    case Nil => min
    case (x,y)::q => if (y<min) minY(q,y) else minY(q,min)
  }

  def maxX(g:Grille,max:Int):Int =g match {
    case Nil => max
    case (x,y)::q => if (x>max) maxX(q,x) else maxX(q,max)
  }

  def maxY(g:Grille,max:Int):Int =g match {
    case Nil => max
    case (x,y)::q => if (y>max) maxY(q,y) else maxY(q,max)
  }

  def coinSupGauche (g: Grille) : (Int, Int) = g match{
    case Nil => throw new Exception("Grille vide")
    case (x,y)::q => (minX(g,x),minY(g,y))
  }


  def coinInfDroite (g: Grille) : (Int, Int) = g match{
    case Nil => throw new Exception("Grille vide")
    case (x,y)::q => (maxX(g,x),maxY(g,y))
  }

  def estX(g : Grille, l : Int, c : Int):Boolean = g match {
    case Nil => false
    case (x,y)::q => if ((x,y) == (l,c)) true else estX(q,l,c)
  }

  def afficherGrille(g:Grille):Unit={
    def aux(ligne:Int, colonne:Int):Unit={
      if(!(ligne>coinInfDroite(g)._1)){
        if(colonne>coinInfDroite(g)._2){
          print("\n")
          aux(ligne+1, coinSupGauche(g)._2)
        }else{
          if(estX(g,ligne, colonne)){
            print("X")
            aux(ligne, colonne+1)
          }else{
            print(" ")
            aux(ligne, colonne+1)
          }
        }
      }
    }
    aux(coinSupGauche(g)._1,coinSupGauche(g)._2)
  }

  //Question 3
  def voisines8(l:Int, c:Int):List[(Int, Int)]={
    (l-1,c-1)::(l-1,c)::(l-1,c+1)::(l,c-1)::(l,c+1)::(l+1,c-1)::(l+1,c)::(l+1,c+1)::Nil
  }

  //Question 4
  def compteVoisines8(g : Grille, l : Int, c : Int):Int={
    (voisines8(l,c) foldLeft 0)((somme,couple) => if(estX(g,couple._1,couple._2)) somme + 1 else somme)
  }

  def survivantes(g:Grille):Grille= {
    g filter (couple => (compteVoisines8(g,couple._1,couple._2) == 2) || (compteVoisines8(g,couple._1,couple._2) == 3))
  }

  //Question 5
  def candidates(g1:Grille):Grille={

    def aux1(g2:Grille, ite:Int):Grille ={

      if(g1==Nil){
        Nil
      }

      def listevoisines:List[(Int,Int)] = voisines8(g1(ite)._1,g1(ite)._2)

      def g3:Grille = g2++( listevoisines filter (X => !g2.contains(X) && !estX(g1,X._1,X._2)))

      if (ite==g1.length-1){
        g3
      } else{
        aux1(g3,ite+1)
      }

    }


    aux1(Nil,0)
  }

  //Question 6
  def naissances(g:Grille):Grille={
    if(g==Nil){
      Nil
    }

    def grilleN:Grille = candidates(g) filter (X=>compteVoisines8(g,X._1,X._2)==3)

    grilleN
  }

  //Question 7
  def jeuDeLaVie(init:Grille, n:Int):Unit={
    println("------")
    println("étape n° "+n)
    println("------")
    if(n>0){
      afficherGrille(init)
      jeuDeLaVie(survivantes(init)++naissances(init).distinct, n-1)
    } else {
      afficherGrille(init)
    }
  }

  //Question 8
  def voisines4(l:Int, c:Int):List[(Int, Int)]={
    (l-1,c)::(l,c-1)::(l,c+1)::(l+1,c)::Nil
  }

  //Question 9
  def naitJDLV(nbVoisines:Int):Boolean = (nbVoisines==3)

  def survitJDLV(nbVoisines:Int):Boolean = (nbVoisines==3 || nbVoisines == 2)

  def naitFREDKLIN(nbVoisines:Int):Boolean = ((nbVoisines % 2) == 1)

  def survitFREDKLIN(nbVoisines:Int):Boolean = ((nbVoisines % 2) == 1)

  //Question 10

  def compteVoisinesG(g : Grille, l : Int, c : Int, voisinage : (Int, Int)=>List[(Int, Int)]):Int={
    (voisinage(l,c) foldLeft 0)((somme,couple) => if(estX(g,couple._1,couple._2)) somme + 1 else somme)
  }

  def survivantesG(g:Grille, regle : (Int)=>Boolean, voisinage : (Int, Int)=>List[(Int, Int)] ):Grille= {
    g filter (couple => (regle(compteVoisinesG(g,couple._1,couple._2, voisinage))))
  }

  def candidatesG(g:Grille, regle : (Int)=>Boolean, voisinage : (Int, Int)=>List[(Int, Int)] ):Grille={
    def aux(grilleDecompte: Grille, g : Grille) : Grille = grilleDecompte match{
      case Nil => Nil
      //case (x,y)::q => cellulesMortes(voisinage(x, y), g, coinSupGauche(g), coinInfDroite(g))++aux(q,g)
    }
    aux(g,g).distinct
  }

  def naissancesG(g:Grille, regle : (Int)=>Boolean, voisinage : (Int, Int)=>List[(Int, Int)] ):Grille={
    def aux1(grille_decompte: Grille, res: Grille): Grille = grille_decompte match {
      case Nil => res
      case (x,y)::q => if(regle(aux2(candidatesG(g,regle,voisinage)))) aux1(q, res.concat((x,y)::Nil)) else aux1(q, res)
    }
    def aux2(l:List[(Int, Int)]): Int = {
      l.intersect(g).length
    }
    aux1(g, List[(Int, Int)]())
  }

  //Question 11

  def moteur(init:Grille, n:Int, regleNaiss : (Int)=>Boolean, regleSurvie : (Int)=>Boolean, voisinage : (Int, Int)=>List[(Int, Int)]):Unit={
    println("------")
    println("étape n° "+n)
    println("------")
    if(n>0){
      afficherGrille(init)
      moteur(survivantesG(init,regleSurvie,voisinage)++naissancesG(init,regleNaiss,voisinage).distinct, n-1,regleNaiss,regleSurvie,voisinage)
    } else {
      afficherGrille(init)
    }
  }

  //Question 12

  def simulationJeuDeLaVie(init:Grille,n:Int):Unit={
    moteur(init, n, naitJDLV, survitJDLV, voisines8)
  }

  def simulationAutomateDeFredklin(init:Grille,n:Int):Unit={
    moteur(init, n, naitFREDKLIN, survitFREDKLIN, voisines4)
  }

  //Question 13

  def voisines4diagonales(l:Int, c:Int):List[(Int, Int)]={
    (l-1,c-1)::(l-1,c+1)::(l+1,c-1)::(l+1,c+1)::Nil
  }

  def simulationVarianteAutomateDeFredklin(init:Grille,n:Int):Unit={
    moteur(init, n, naitFREDKLIN, survitFREDKLIN, voisines4diagonales)
  }

  def main(args: Array[String]): Unit = {
    jeuDeLaVie(chainesToGrille(List("XXX")), 10)
  }
}
