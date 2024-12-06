package Position;

import java.util.Objects;

public class Position {
    public int x;
    public int y;
    private int hashCode;

    public Position(int x, int y) {
        this.x = x;
        this.y = y;
        this.hashCode = Objects.hash(x, y);
    }

    public void move(char direction) {
        switch (direction) {
            case '^':
                this.y++;
                break;
            case '<':
                this.x--;
                break;
            case '>':
                this.x++;
                break;
            case 'v':
                this.y--;
                break;
            default:
                System.out.println("Invalid character: " + direction);
                break;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }

        Position pos = (Position) obj;
        return x == pos.x && y == pos.y;
    }

    @Override
    public int hashCode() {
        return hashCode;
    }

    public void reHash() {
        this.hashCode = Objects.hash(x, y);
    }
}