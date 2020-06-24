## NN Regression Model

Neural Network Regression Model

Load Data: I simply saved data boxing prepared to trainyear.csv testyear.csv and validateyear.csv please change the path parameter **pwd** to the directory of file in your local env  

Famework: Tensorflow and Keras

Package to download: Numpy, Matplotlib, Pandas, Tensorflow, Keras

Nodes of each layer : 49 98 147 98 1

Activation function: ReLu

**Note**: Network structure is EASY to modify in  #hyperparameters setting section in notebook file **elec complete**



## Results

I provide all plots and evaluation metrics similar to Boxi's train/test/validation setup in order to make the model comparison work easier.

Plot resolution can be adjusted in result visualization section(but you have to run the code again, to be updated)

![2003prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2003prediction.png)

![2004prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2004prediction.png)

![2005prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2005prediction.png)

![2006prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2006prediction.png)

![2007prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2007prediction.png)

![2008prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2008prediction.png)

![2009prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2009prediction.png)

![2010prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2010prediction.png)

![2011prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2011prediction.png)

![2012prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2012prediction.png)

![2013prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2013prediction.png)

![2014prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2014prediction.png)

![2015prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2015prediction.png)

![2016prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/2016prediction.png)

![MAE hourly prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/MAE hourly prediction.png)

![MAE Year prediction](/Users/zhongyuanzhang/Dropbox/power_share/jasper_NN/elec/plot/MAE Year prediction.png)

## Discussion

1. The network structure is determined by trial(black box), currently i gave a relatively good network among all my trials. More complicated network structures will lead to model overfitting on both validation set and testing set.
2. I proposed to try AutoKeras framework that will provide auto-tuned network structures. Hopefully we can have a better prediction performance but even worse intepretability.