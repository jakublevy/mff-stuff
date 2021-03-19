#include <iostream>
#include "../include/Matrix.h"
#include "../include/InputParser.h"

using std::vector;
using std::size_t;
using std::cout;

template <typename T>
Matrix<T> constructAMatrix(const vector<InputData>& input);

template <typename T>
Matrix<T> constructBVector(const vector<InputData>& input);

int main() {
    InputParser parser("../input.txt");
    vector<InputData> input = parser.read();
    Matrix<double> A = constructAMatrix<double>(input);
    Matrix<double> b = constructBVector<double>(input);
    Matrix<double> coeffs = A.lsm(b);
    cout << "α = " << coeffs[{1,1}] << '\n' << "β = " << coeffs[{2,1}] << '\n' << "γ = " << coeffs[{3,1}] << '\n';
    return 0;
}

template <typename T>
Matrix<T> constructAMatrix(const vector<InputData>& input) {
    Matrix<double> a(input.size(), 3);
    for(size_t i = 1; i <= a.height(); ++i) {
        a[{i, 1}] = input[i - 1].height() * input[i - 1].height();
        a[{i, 2}] = input[i - 1].height();
        a[{i, 3}] = 1;
    }
    return a;
}

template <typename T>
Matrix<T> constructBVector(const vector<InputData>& input) {
    Matrix<T> b(input.size(), 1);
    for(size_t i = 1; i <= b.height(); ++i)
        b[{i, 1}] = input[i - 1].weight();

    return b;
}