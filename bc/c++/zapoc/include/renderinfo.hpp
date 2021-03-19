#ifndef RENDERINFO_HPP
#define RENDERINFO_HPP

#include "lsystem.hpp"
#include <QChar>
#include <QColor>
#include <QPointF>
#include <QVector2D>
#include "state.hpp"

//This class contains all the information needed for fractal to be drawn.

class RenderInfo
{
public:
    RenderInfo(Lsystem &&lsys, std::map<QChar, QColor> &&colorChanges, int rotationAngle, int directionAngle, int lineLength, QPointF const &location)
                : _lsys(std::move(lsys)), _colorChanges(std::move(colorChanges)),
                  _rotationAngle(rotationAngle), _directionAngle(std::move(directionAngle)),
                  _lineLength(lineLength), _location(location) { }

    QString const &sentence() const noexcept { return _lsys.sentence(); }
    QColor colorOf(QChar c) const;
    int rotationAngle() const noexcept { return _rotationAngle; }
    int lineLength() const noexcept { return _lineLength; }

    int directionAngle() const noexcept { return _directionAngle; }
    QPointF const &location() const noexcept { return _location; }

    void swap(RenderInfo &&other) noexcept;

private:
    Lsystem _lsys;
    std::map<QChar, QColor> _colorChanges;
    int _rotationAngle;
    int _directionAngle;
    int _lineLength;
    QPointF _location;

};

#endif // RENDERINFO_HPP
