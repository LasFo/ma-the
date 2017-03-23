import matplotlib.pyplot as plt
from numpy import genfromtxt 


x = genfromtxt('GHCScaling',delimiter=',')
y = genfromtxt('LAScaling',delimiter=',')
b = genfromtxt('WSLScaling',delimiter=',')
a = genfromtxt('PScaling',delimiter=',')

#print(x)
f = plt.subplot()
f.plot(y[:,0],y[:,1],ls='--',c='black',linewidth=5)
f.plot(b[:,0],b[:,1],ls='-.',c='gray')
f.plot(a[:,0],a[:,1],ls=':',c='dimgrey')
f.plot(x[:,0],x[:,1],ls='-',c='slategrey')
plt.legend(['LA','WSL','Project','GHC'],fontsize=12)
f.set_ylabel('seconds',fontsize=25)
f.set_xlabel('threads')
plt.show()
