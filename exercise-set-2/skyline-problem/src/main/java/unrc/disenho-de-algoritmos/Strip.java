package skyline;
/**
* Class that represents the strips of a skyline.
* A skyline is an array (or ArrayList in this case) of strips.
* @author Agustin Borda.
*/
public class Strip {
  private int place;
  private int heigth;

  /**
  * Default constructor.
  */
  public Strip() {
    place = 0;
    heigth = 0;
  }

  /**
  * Parametrized constructor.
  * @param p The place of the strip.
  * @param h The heigth of the strip.
  */
  public Strip(int p, int h) {
    place = p;
    heigth = h;
  }

  /**
  * Gets the place of the strip.
  * @return The place of the strip.
  */
  public int getPlace() {
    return place;
  }

  /**
  * Gets the heigth of the strip.
  * @return The heigth of the strip.
  */
  public int getHeigth() {
    return heigth;
  }

  /**
  * Sets the place of the strip.
  * @param p The new place of the strip.
  */
  public void setPlace(int p) {
    place = p;
  }

  /**
  * Sets the heigth of the strip.
  * @param h The new heigth of the strip.
  */
  public void setHeigth(int h) {
    heigth = h;
  }

  /**
  * Compares the current object with another.
  * @param obj The object that we will compare with the current one.
  * @return True if the object are equals, false otherwise.
  */
  @Override
  public boolean equals(Object obj) {
    Strip st = (Strip)obj;
    return (this.getPlace() == st.getPlace()) && (this.getHeigth() == st.getHeigth());
  }

  /**
  * Converts the current object in a String.
  * @return The Object as a String.
  */
  @Override
  public String toString() {
    return "("+this.getPlace()+","+this.getHeigth()+")";
  }
}
