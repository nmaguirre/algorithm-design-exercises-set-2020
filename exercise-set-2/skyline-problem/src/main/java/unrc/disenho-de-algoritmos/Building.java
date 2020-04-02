package skyline;
/**
* Class that represents a building for the skyline problem.
* @author Agustin Borda.
*/
public class Building {
  private int left;
  private int right;
  private int heigth;

  /**
  * Default constructor.
  */
  public Building() {
    left = 0;
    right = 0;
    heigth = 0;
  }

  /**
  * Parametrized constructor.
  * @param l The left side of the building.
  * @param r The right side of the building.
  * @param h The heigth of the building.
  */
  public Building(int l, int r, int h) {
    left = l;
    right = r;
    heigth = h;
  }

  /**
  * Gets the left side of the building.
  * @return The left side of the building.
  */
  public int getLeft() {
    return left;
  }

  /**
  * Gets the right side of the building.
  * @return The right side of the building.
  */
  public int getRight() {
    return right;
  }

  /**
  * Gets the heigth of the building.
  * @return The height of the building.
  */
  public int getHeigth() {
    return heigth;
  }

  /**
  * Sets the left side of the building.
  * @param l The new left side of the building.
  */
  public void setLeft(int l) {
    left = l;
  }

  /**
  * Sets the right side of the building.
  * @param r The new right side of the building.
  */
  public void setRight(int r) {
    right = r;
  }

  /**
  * Sets the heigth of the building.
  * @param h The new heigth of the building.
  */
  public void setHeigth(int h) {
    heigth = h;
  }

}
