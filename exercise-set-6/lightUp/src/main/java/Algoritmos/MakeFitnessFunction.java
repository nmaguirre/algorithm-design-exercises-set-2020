package Algoritmos;

import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.IChromosome;

/**
 * Clase para definir la fitnness function.
 * @author Yanina Celi y Agustin Borda
 */
public class MakeFitnessFunction extends FitnessFunction {

  //Tablero que sirve de auxiliar para calcular la funcion fitness
  private final Casilla[][] board;

  /**
   * Contructor, inicializa el atributo "board".
   * @param b : tablero que se le asigna al atributo "board" : Casilla[][]
  */
  public MakeFitnessFunction(Casilla[][] b) {
    board = b;
  }
    
  /**
   *Fitness function (evalua un cromosoma).
   * @pre board != null && c != null
   * @param c : el cromosoma a evaluar : IChromosome
   * @see getCantFocos
   * @see getCantIluminadas
   * @see getCantConflictos
   * @return valor asignado por la funcion fitness : double
  */
  @Override
  protected double evaluate(IChromosome c) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }
 
  /**
   * se ponen los elementos de c en el tablero board 
   * (en las casillas blancas del mismo) y se cuentan la cantidad de focos que hay. 
   * @pre board != null && c != null
   * @param c : cromosoma a evaluar en la funcion fitness : IChromosome
   * @return cantidad de focos que hay en el cromosoma : int
  */
  public int getCantFocos(IChromosome c) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }

  /**
   * Retorna la cantidad de focos que tiene el tablero b.
   * @pre board != null && c != null
   * @param b : tablero al que se le va a calcular la cantidad de focos : Casilla[][]
   * @return cantidad de focos que tiene el tablero b : int
   */
  public static int getFocos(Casilla[][] b) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }

  /**
   * Se ponen los elementos de c en el tablero board
   *  (en las casillas blancas del mismo) y se calculan las casilas iluminadas.
   *  @pre board != null && c != null
   * @param c : cromosoma a evaluar en la funcion fitness : IChromosome
   * @see inicializarTablero
   * @see iluminarTablero
   * @return cantidad de casillas iluminadas (focos incluidos) en el tablero board : int
  */
  public int getCantIluminadas(IChromosome c) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }

  /**
   * se crea una matriz de booleanos, donde se marca con true 
   * las posiciones que en el tablero board estan iluminadas. 
   * luego se informa si la casilla de la posicion (fila,columna) esta iluminada
   * @pre c != null && 0<=i<7 && 0<=j<7
   * @param fila : fila de c en donde se encuentra la casilla a tratar : int
   * @param columna : columna de c en donde se encuentra la casilla a tratar : int
   * @param c : tablero donde se encuentra la casilla a tratar : Casilla[][]
   * @see inicializarTablero
   * @see iluminarTablero
   * @return valor de verdad de que esta iluminada la casilla en (fila,columna) de c : boolean
  */
  public static boolean estaIluminada(int fila,int columna,Casilla[][] c) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }
 
  /**
   * inicializa la matriz iluminadas con falso.
   * @pre true
   * @param iluminadas : matriz a inicializar con "false" : Boolean[][]
  */
  public static void inicializarTablero(Boolean[][] iluminadas) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }

  /**
   * Pone true en la matriz iluminadas,
   *  dependiendo si la casilla de esa posicion,  en board , 
   *  esta iluminada por el foco que hay en (i,j).
   * @param board : tablero con focos y casillas negras : Casilla[][]
   * @param iluminadas : matriz de booleanos a modificar, inicializada en "false" : Boolean[][]
   * @param i : fila de board donde esta el foco : int
   * @param j : columna de board donde esta el foco : int
   * @see iluminarArriba
   * @see iluminarAbajo
   * @see iluminarDerecha
   * @see iluminarIzquierda
  */
  public static void iluminarTablero(Casilla[][] board, Boolean[][] iluminadas, int i,int j) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }


  /**
   * crea un tablero apartir de "board" y pone en las casillas blancas del mismo, 
   * los elementos del cromosoma c.
   * @param c : cromosoma a tratar : IChromosome
   * @return tablero board con los elementos de c : Casilla[][]
  */
  public Casilla[][] completeBoard(IChromosome c) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }


  /**
   * calcula los conflictos de lugar 
   *(si hay mas de un foco en la misma fila o columna) en el tablero aux.
   * @param aux : tablero a tratar : Casilla[][]
   * @return cantidad de conflictos de lugar en aux : int
  */
  public static int conflictosLugar(Casilla[][] aux) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }


  /**
   * calcula la cantidad de conflictos con casillas negras en aux
   * (si hay alguna casilla negra que tenga mas o menos focos de los que inidica).
   * @param aux : tablero a tratar : Casilla[][]
   * @return cantidad de conflictos con casillas negras en aux : int
  */
  public static int conflictosCasillasNegras(Casilla[][] aux) {
    throw new UnsupportedOperationException("Method not yet implemented!");
  }


}
