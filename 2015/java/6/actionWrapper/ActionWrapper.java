package ActionWrapper;

import Position.Position;

public class ActionWrapper {
    public String action;
    public Position start;
    public Position end;

    public ActionWrapper() {
        this.action = "";
        this.start = new Position(0, 0);
        this.end = new Position(0, 0);
    }

    public void parse(String line) {
        String[] parts = line.split(" ");

        if (parts.length == 5) {
            this.action = parts[1];
            String coords = parts[2];
            String[] coordsParts = coords.split(",");
            this.start = new Position(Integer.parseInt(coordsParts[0]), Integer.parseInt(coordsParts[1]));

            coords = parts[4];
            coordsParts = coords.split(",");
            this.end = new Position(Integer.parseInt(coordsParts[0]), Integer.parseInt(coordsParts[1]));
        } else {
            this.action = parts[0];
            String coords = parts[1];
            String[] coordsParts = coords.split(",");
            this.start = new Position(Integer.parseInt(coordsParts[0]), Integer.parseInt(coordsParts[1]));

            coords = parts[3];
            coordsParts = coords.split(",");
            this.end = new Position(Integer.parseInt(coordsParts[0]), Integer.parseInt(coordsParts[1]));
        }
    }
}
