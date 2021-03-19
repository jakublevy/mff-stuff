#ifndef TURTLE_HPP
#define TURTLE_HPP

#include "renderinfo.hpp"
#include "drawinginstruction.hpp"
#include <memory>

//This class prepares rendering information for direct usage with QPainter in class DrawingWidget.

class Turtle
{
public:
    Turtle(RenderInfo &&info) : info(std::move(info)) { }

    //From all rendering information needed, this creates a vector of instructions for direct usage with QPainter.
    std::unique_ptr<std::vector<DrawingInstruction>> instructions() const;

private:
    RenderInfo info;

    //Returns next position from supplied parameters.
    static QPointF newLocation(QPointF const &pt, qreal const &angle, int distance) noexcept;

    static qreal convertToRadians(int angle) noexcept;

    static constexpr auto pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286;
};

#endif // TURTLE_HPP
