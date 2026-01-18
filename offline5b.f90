import numpy as np


def p(x):
    return -(x+1)

def q(x):
    return 2

def r(x):
    return (1-x**2)*np.exp(-x)

def f1(x,y,z):
    return p(x)*z + q(x)*y + r(x)

def g1(x,y,z):
    return z

def f2(x,y,z):
    return p(x)*z + q(x)*y

def g2(x,y,z):
    return z

def euler(a, b, h, y0, z0, f, g):
    n = int((b - a)/h) + 1
    x = [a + i*h for i in range(n)]
    y = [0 for _ in range(n)]
    z = [0 for _ in range(n)]
    y[0] = y0
    z[0] = z0
    for i in range(n-1):
        y[i+1] = y[i] + h * g(x[i], y[i], z[i])
        z[i+1] = z[i] + h * f(x[i], y[i], z[i])
    
    return y

def rk4(a, b, h, y0, z0, f, g):
    n = int((b - a)/h) + 1
    x = [a + i*h for i in range(n)]
    y = [0 for _ in range(n)]
    z = [0 for _ in range(n)]
    y[0] = y0
    z[0] = z0
    for i in range(n-1):
        # k1 = h*f(t[i],y[i])
        # k2 = h*f(t[i]+h/2,y[i]+k1/2)
        # k3 = h*f(t[i]+h/2,y[i]+k2/2)
        # k4 = h*f(t[i]+h,y[i]+k3)
        # y[i+1] = y[i] + (k1+2*k2+2*k3+k4)/6
        
        k1y = h * g(x[i], y[i], z[i])
        k1z = h * f(x[i], y[i], z[i])
        k2y = h * g(x[i] + h/2, y[i] + k1y/2, z[i] + k1z/2)
        k2z = h * f(x[i] + h/2, y[i] + k1y/2, z[i] + k1z/2)
        k3y = h * g(x[i] + h/2, y[i] + k2y/2, z[i] + k2z/2)
        k3z = h * f(x[i] + h/2, y[i] + k2y/2, z[i] + k2z/2)
        k4y = h * g(x[i] + h, y[i] + k3y, z[i] + k3z)
        k4z = h * f(x[i] + h, y[i] + k3y, z[i] + k3z)

        y[i+1] = y[i] + (k1y + 2*k2y + 2*k3y + k4y)/6
        z[i+1] = z[i] + (k1z + 2*k2z + 2*k3z + k4z)/6
    
    return y

a = 0
b = 1
h = 0.2
alpha = 0
beta = 1

y1_0 = -1
z1_0 = 0

y2_0 = 0
z2_0 = 1

# y1 = euler(a, b, h, y1_0, z1_0, f1, g1)
# y2 = euler(a, b, h, y2_0, z2_0, f2, g2)

y1 = rk4(a, b, h, y1_0, z1_0, f1, g1)
y2 = rk4(a, b, h, y2_0, z2_0, f2, g2)

n = len(y1)
x = [a + i*h for i in range(n)]
y = [0 for _ in range(n)]
yex = [0 for _ in range(n)]
error = [0 for _ in range(n)]
d = 6

for i in range(n):
    y[i] = y1[i] + ((beta - y1[n-1])/y2[n-1] ) * y2[i]
    # yex[i] = actual(x[i])
    # error[i] = abs(y[i] - yex[i])
    # print(round(x[i], d), round(y[i], d), round(yex[i], d), round(error[i], d))
    print(round(x[i], d), round(y[i], d))