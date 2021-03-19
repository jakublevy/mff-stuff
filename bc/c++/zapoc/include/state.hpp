#ifndef STATE_HPP
#define STATE_HPP

#include <QPointF>

//Represents a state of a turtle (location, orientation).

class State
{
public:
    State(QPointF const &location, int directionAngle) : _location(location), _directionAngle(directionAngle) { }

    QPointF const &location() const noexcept { return _location; }
    void setLocation(QPointF const &location) { _location = location; }
    void setLocation(QPointF &&location) {_location = std::move(location); }

    int directionAngle() const noexcept { return _directionAngle; }
    void setDirectionAngle(int directionAngle) { _directionAngle = directionAngle; }

private:
    QPointF _location;
    int _directionAngle;
};

#endif // STATE_HPP
