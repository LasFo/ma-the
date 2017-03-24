import matplotlib.pyplot as plt
from numpy import genfromtxt 


x = genfromtxt('GHCScaling2',delimiter=',')
y = genfromtxt('LAScaling2',delimiter=',')
b = genfromtxt('WSLScaling2',delimiter=',')
a = genfromtxt('PScaling2',delimiter=',')

#print(x)
f = plt.subplot()
f.plot(y[:,0],y[:,1],ls='--',c='black',linewidth=4)
f.plot(b[:,0],b[:,1],ls='-.',c='gray',linewidth=4)
f.plot(a[:,0],a[:,1],ls=':',c='dimgrey',linewidth=4)
f.plot(x[:,0],x[:,1],ls='-',c='slategrey',linewidth=4)
plt.legend(['LA','WSL','Project','GHC'],fontsize=20)
f.set_ylabel('seconds',fontsize=20)
f.set_xlabel('threads',fontsize=20)
plt.show()
