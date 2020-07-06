# sigFeature
sigFeature: sigFeature is an R package which is able to find out the significant features using support vector machine recursive feature elimination method (SVM-RFE)  (Guyon, I., et al. 2002) and t-statistic. Feature selection is an important  part dealing with machine learning technology. SVM-RFE is recognized as one  of the most effective filtering methods, which is based on a greedy algorithm that only finds the best possible combination for classification without  considering the differentially significant features between the classes.  To overcome this limitation of SVM-RFE, the proposed approach is tuned to  find differentially significant features along with notable classification accuracy. This package is able to enumerate the feature selection of any  two-dimensional (for binary classification) data such as a micro array etc.  This vignette explains the use of the package in a publicly available  micro array data set.

Abstract: Biological data are accumulating at a faster rate, but interpreting them still remains a problem. Classifying biological data into distinct groups is the first step in understanding them. Data classification in response to a certain treatment is an extremely important aspect for differentially expressed genes in making present/absent calls. Many feature selection algorithms have been developed including the support vector machine recursive feature elimination procedure (SVM-RFE) and its variants. Support vector machine RFEs are greedy methods that attempt to find superlative possible combinations leading to binary classification, which may not be biologically significant. To overcome this limitation of SVM-RFE, we propose a novel feature selection algorithm, termed as “sigFeature” (https://bioconductor.org/packages/sigFeature/), based on SVM and t statistic to discover the differentially significant features along with good performance in classification. The “sigFeature” R package is centered around a function called “sigFeature,” which provides automatic selection of features for the binary classification. Using six publicly available microarray data sets (downloaded from Gene Expression Omnibus) with different biological attributes, we further compared the performance of “sigFeature” to three other feature selection algorithms. A small number of selected features (by “sigFeature”) also show higher classification accuracy. For further downstream evaluation of its biological signature, we conducted gene set enrichment analysis with the selected features (genes) from “sigFeature” and compared it with the outputs of other algorithms. We observed that “sigFeature” is able to predict the signature of four out of six microarray data sets accurately, whereas the other algorithms predict less data set signatures. Thus, “sigFeature” is considerably better than related algorithms in discovering differentially significant features from microarray data sets.
The link of the article is given below.
https://www.frontiersin.org/articles/10.3389/fgene.2020.00247/full