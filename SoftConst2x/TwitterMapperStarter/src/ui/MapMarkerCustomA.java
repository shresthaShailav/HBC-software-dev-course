package ui;

import com.sun.org.glassfish.external.statistics.Stats;
import jdk.nashorn.internal.runtime.regexp.joni.constants.StackType;
import org.openstreetmap.gui.jmapviewer.*;
import twitter4j.Status;
import util.Util;

import java.awt.*;
import java.awt.image.BufferedImage;

public class MapMarkerCustomA extends MapMarkerCircle {
    public static final double defaultMarkerSize = 20.0;
    public static Color defaultColor = Color.BLUE;
    private Status status;

    public MapMarkerCustomA(Layer layer, Coordinate coord, Color color, Status receivedStatus) {
        super(layer, (String) null, coord, defaultMarkerSize, STYLE.FIXED, getDefaultStyle());
        this.status = receivedStatus;
        setColor(Color.BLACK);
        setBackColor(color);
        BufferedImage userPic = Util.imageFromURL(receivedStatus.getUser().getProfileImageURL());
        Graphics2D profileImage = userPic.createGraphics();
        profileImage.drawImage(userPic, null, 0, 0 );
        paint(profileImage, new Point(0, 0), (int) getRadius());
    }

    private Style getCustomStyle(Layer layer, Color color, Status status) {
        Style customStyle = new Style();
        customStyle.setColor(Color.BLACK);
        customStyle.setBackColor(color);
        return customStyle;
    }

    public Status getStatus() {
        return status;
    }
}
