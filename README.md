# chi
这个程序用于计算各个分波相移的chisquare  
我们只需要编译chi.f文件即可  
chiset.d是配置文件，各个参数写得比较明白了  
phaseexp.d是实验值，这个可以不修改  
phasetheory.d是理论计算的值，每次按照既有文件的格式更改即可  
>注意：这个列数可以超过set中设定的，程序只计算前面这些列
# partransverse
这个程序用于对contact terms中的LECs的动量空间和分波表象下参数进行转换  
transverse.ipynb是用于转换的python主程序，其中ptolsj是动量到lsj反之是lsjtop  
被转换的参数输入到表格input_vector中，是一个24维的列向量  
以ptolsj为例，记transverse_matrix和reorder对应的矩阵为A、B，列向量对应的每个分量的含义列表如下  
| BAp-par(lsj)                      | Ap-par                            | p_par |
|-----------------------------------|-----------------------------------|-------|
| $\widetilde{C}_{^1S_0}^{np}$    | $\widetilde{C}_{^1S_0}^{np}$    | cs    |
| $C_{^1S_{0}}$                     | $\widetilde{C}_{^3S_1}$         | ct    |
| $\widehat{D}_{^{1}S_0}$         | $C_{^1S_{0}}$                     | c1    |
| $D_{^1S_0}$                       | $C_{^3P_0}$                       | c2    |
| $C_{^3P_0}$                       | $C_{^1P_1}$                       | c3    |
| $D_{^3P_0}$                       | $C_{^3P_1}$                       | c4    |
| $C_{^1P_1}$                       | $C_{^3S_{1}}$                     | c5    |
| $D_{^1P_1}$                       | $C_{^3S_1-^3D_1}$                 | c6    |
| $C_{^3P_1}$                       | $C_{^3P_2}$                       | c7    |
| $D_{^3P_1}$                       | $\widehat{D}_{^{1}S_0}$         | d1    |
| $\widetilde{C}_{^3S_1}$         | $D_{^1S_0}$                       | d2    |
| $C_{^3S_{1}}$                     | $D_{^3P_0}$                       | d3    |
| $\widehat{D}_{^3S_1}$             | $D_{^1P_1}$                       | d4    |
| $D_{^3S_1}$                       | $D_{^3P_1}$                       | d5    |
| $D_{^3D_1}$                       | $\widehat{D}_{^3S_1}$             | d6    |
| $C_{^3S_1-^3D_1}$                 | $D_{^3S_1}$                       | d7    |
| $\widehat{D}_{^3S_1-^3D_1}$ | $D_{^3D_1}$                       | d8    |
| $D_{{}^{3}S_{1}-^3D_1}$           | $\widehat{D}_{^3S_1-^3D_1}$ | d9    |
| $D_{^1D_2}$                       | $D_{{}^{3}S_{1}-^3D_1}$           | d10   |
| $D_{^3D_2}$                       | $D_{^1D_2}$                       | d11   |
| $C_{^3P_2}$                       | $D_{^3D_2}$                       | d12   |
| $D_{^3P_2}$                       | $D_{^3P_2}$                       | d13   |
| $D_{^3P_2-^3F_2}$                 | $D_{^3P_2-^3F_2}$                 | d14   |
| $D_{^3D_3}$                       | $D_{^3D_3}$                       | d15   |
