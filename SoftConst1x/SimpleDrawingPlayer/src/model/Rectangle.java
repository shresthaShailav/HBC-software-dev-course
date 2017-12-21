package model;

import sound.MidiSynth;

import java.awt.*;

public class Rectangle extends Shape {
    public Rectangle(Point topLeft, MidiSynth midiSynth) {
        super(topLeft, midiSynth);
        instrument = 27;
    }

    // EFFECTS: return true iff the given x value is within the bounds of the Rectangle
    public boolean containsX(int x){
        return (this.x <= x) && (x <= this.x + width);
    }

    // EFFECTS: return true iff the given y value is within the bounds of the Rectangle
    public boolean containsY(int y) {
        return (this.y <= y) && (y <= this.y + height);
    }

    // EFFECTS: return true if the given Point (x,y) is contained within the bounds of this Rectangle
    public boolean contains(Point point) {
        int point_x = point.x;
        int point_y = point.y;

        return containsX(point_x) && containsY(point_y);
    }

    // REQUIRES: the x,y coordinates of the Point are larger than the x,y coordinates of the shape
    // MODIFIES: this
    // EFFECTS:  sets the bottom right corner of this Shape to the given Point
    public void setBounds(Point bottomRight) {
        width  = bottomRight.x - x;
        height = bottomRight.y - y;
    }


    //EFFECTS: draws the shape
    protected void drawGraphics(Graphics g) {
        g.drawRect(x, y, width, height);
    }


    //EFFECTS: fills the shape
    protected void fillGraphics(Graphics g) {
        g.fillRect(x, y, width, height);
    }

    protected int getArea() {
        return width * height;
    }




}
