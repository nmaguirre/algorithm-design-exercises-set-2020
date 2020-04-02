package skyline;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;


import java.util.List;
import java.util.ArrayList;

import org.junit.Before;
import org.junit.Assert;
import org.junit.Test;

public class SkylineTest {
  private ArrayList<Building> input;
  private ArrayList<Strip> expected;
  private ArrayList<Strip> emptyStrip;
  private ArrayList<Building> bigInput;
  private ArrayList<Strip> bigExpected;
  private ArrayList<Strip> merge1;
  private ArrayList<Strip> merge2;
  private ArrayList<Strip> merged;

  @Before
  public void setUp() {
    input = new ArrayList<Building>();
    input.add(new Building(1,5,11));
    expected = new ArrayList<Strip>();
    expected.add(new Strip(1,11));
    expected.add(new Strip(5,0));
    emptyStrip = new ArrayList<Strip>();
    emptyStrip.add(new Strip());
    bigInput = new ArrayList<Building>();
    bigInput.add(new Building(1,4,4));
    bigInput.add(new Building(3,6,2));
    bigInput.add(new Building(10,12,4));
    bigInput.add(new Building(13,20,2));
    bigInput.add(new Building(14,16,5));
    bigInput.add(new Building(17,18,1));
    bigExpected = new ArrayList<Strip>();
    bigExpected.add(new Strip(1,4));
    bigExpected.add(new Strip(4,2));
    bigExpected.add(new Strip(6,0));
    bigExpected.add(new Strip(10,4));
    bigExpected.add(new Strip(12,0));
    bigExpected.add(new Strip(13,2));
    bigExpected.add(new Strip(14,5));
    bigExpected.add(new Strip(16,2));
    bigExpected.add(new Strip(20,0));
    merge1 = new ArrayList<Strip>();
    merge2 = new ArrayList<Strip>();
    merged = new ArrayList<Strip>();
    merge1.add(new Strip(1,4));
    merge1.add(new Strip(4,0));
    merge2.add(new Strip(3,2));
    merge2.add(new Strip(6,0));
    merge2.add(new Strip(10,4));
    merge2.add(new Strip(12,0));
    merged.add(new Strip(1,4));
    merged.add(new Strip(4,2));
    merged.add(new Strip(6,0));
    merged.add(new Strip(10,4));
    merged.add(new Strip(12,0));

  }

  @Test
  public void testOneElement() {
    assertEquals(Skyline.skyline(input),expected);
  }

  @Test
  public void testEmptyArray() {
    assertEquals(Skyline.skyline(new ArrayList<Building>()),emptyStrip);
  }

  @Test(expected = NullPointerException.class)
  public void testNullArray() {
    Skyline.skyline(null);
  }

  @Test
  public void mergeTest() {
    Assert.assertEquals(Skyline.mergeSkylines(merge1,merge2),merged);
  }

  @Test
  public void testBigInput() {
    assertEquals(Skyline.skyline(bigInput),bigExpected);
  }



}
