package Tetris;
import java.util.Random;
public class Shape {
    protected enum Tetrominoe {
        NoShape,
        Zee,
        Ess,
        Long,
        Stack,
        Square,
        El,
        Jay
    }
    private Tetrominoe pieceShape;
    private int[][] coordinates;
    public Shape() {
        coordinates = new int[4][2];
        setShape(Tetrominoe.NoShape);
    }
    void setShape(Tetrominoe shape) {
        int[][][] coordinateArray = new int[][][]{
                {
                        {0, 0},
                        {0, 0},
                        {0, 0},
                        {0, 0}
                },
                {
                        {0, -1},
                        {0, 0},
                        {-1, 0},
                        {-1, 1}
                },
                {
                        {0, -1},
                        {0, 0},
                        {1, 0},
                        {1, 1}
                },
                {
                        {0, -1},
                        {0, 0},
                        {0, 1},
                        {0, 2}
                },
                {
                        {-1, 0},
                        {0, 0},
                        {1, 0},
                        {0, 1}
                },
                {
                        {0, 0},
                        {1, 0},
                        {0, 1},
                        {1, 1}
                },
                {
                        {-1, -1},
                        {0, -1},
                        {0, 0},
                        {0, 1}
                },
                {
                        {1, -1},
                        {0, -1},
                        {0, 0},
                        {0, 1}
                }
        };
        for (int i = 0; i < 4; i++) {
            System.arraycopy(coordinateArray[shape.ordinal()], 0, coordinates, 0, 4);
        }
        pieceShape = shape;
    }
    Tetrominoe getShape() {
        return pieceShape;
    }
    public void setX(int shapeIndex, int xpos) {
        coordinates[shapeIndex][0] = xpos;
    }
    public void setY(int shapeIndex, int ypos) {
        coordinates[shapeIndex][1] = ypos;
    }
    int getX(int shapeIndex) {
        return coordinates[shapeIndex][0];
    }
    int getY(int shapeIndex) {
        return coordinates[shapeIndex][1];
    }
    void setRandomShape() {
        Random randomObj = new Random();
        int x = (Math.abs(randomObj.nextInt()) % 7) + 1;

        Tetrominoe[] values = Tetrominoe.values();
        setShape(values[x]);
    }
    public int minX() {
        int m = coordinates[0][0];

        for (int i = 0; i < 4; i++) {
            m = Math.min(m, coordinates[i][0]);
        }

        return m;
    }
    int minY() {
        int m = coordinates[0][1];

        for (int i = 0; i < 4; i++) {
            m = Math.min(m, coordinates[i][1]);
        }

        return m;
    }
    Shape rotateLeft() {
        if (pieceShape == Tetrominoe.Square) {
            return this;
        }
        Shape result = new Shape();
        result.pieceShape = pieceShape;

        for (int i = 0; i < 4; i++) {
            result.setX(i, getY(i));
            result.setY(i, -getX(i));
        }
        return result;
    }
    Shape rotateRight() {
        if (pieceShape == Tetrominoe.Square) {
            return this;
        }
        Shape result = new Shape();
        result.pieceShape = pieceShape;

        for (int i = 0; i < 4; i++) {
            result.setX(i, -getY(i));
            result.setY(i, getX(i));
        }
        return result;
    }
}
