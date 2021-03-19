#ifndef DRAWINGINSTRUCTIONS_HPP
#define DRAWINGINSTRUCTIONS_HPP

#include <QPointF>
#include <QColor>

//This class represents one drawing primitive.

class DrawingInstruction
{
public:
    DrawingInstruction(QPointF const &start, QPointF const &end, QColor const &color) : _start(start), _end(end), _color(color) { }

    QPointF const &start() const noexcept { return _start; }
    QPointF const &end() const noexcept { return _end; }
    QColor const &color() const noexcept { return _color; }

private:
    QPointF _start;
    QPointF _end;
    QColor _color;
};

#endif // DRAWINGINSTRUCTIONS_HPP
