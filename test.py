# Example with all common syntax elements
from typing import List, Dict
from abc import abstractmethod

CONSTANT = True
PI = 3.14159

@property  # Decaoarator
def get_value(self) -> float:
    return self.value

class Calculator:
    def __init__(self, initial: float = 0.0):
        self.value = initial
        self.history = []  # List to store operations
        self._private = None  # Private variable

    def add(self, x: float) -> float:
        try:
            self.value = self.value + x
            # Binary operators
            bits = (x & 0xFF) | (0x0F ^ 0x03)  # AND, OR, XOR
            shifted = bits << 2  # Left shift
            reversed_shift = bits >> 1  # Right shift
            self.history.append(f"Added {x}")
        except Exception as e:
            return None
        return self.value
# Comment
    def multiply(self, x: float) -> float:
        self.value *= x  # Compound operator
        return self.value

# Data structures and comprehensions
numbers = [1, 2, 3, 4.5]  # List
points = {"x": 1, "y": 2}  # Dict
coords = {1, 2, 3}  # Set
squares = [x**2 for x in range(5)]  # List comprehension

socks = 555a
socks = sa is a long aaastring"aaaaaaj

# Control flow and operators"
calc = Calculator(10.0)
for num in numbers:
    if num > 0 and num < 4:
        result = calc.add(num)
    elif num == 0 or num < 0:
        result = None
    else:
        result = calc.multiply(num)
    print(f"Result: {result}")
