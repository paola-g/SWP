# Strong-weak pruning for brain network identification in connectome-wide neuroimaging

SWP is a network thresholding method that can be used to remove noisy connections by considering both edge weights and network structure. The algorithm selects both strong edges (i.e. edges belonging to shortest paths) and weak edges that are topologically relevant in weakly connected subnetworks. We also provide a method to evaluate whether networks properties vary between groups of subjects, rather than for each subject individually. In order to perform this comparison, first a mask is created for each group using a Borda voting scheme to integrate all individual networks. Then SWP is applied to the group (masked) networks obtaining three different pruned network masks. The final mask is computed as the union of the edges in all of the group specific masks. This allowed to obtain subject networks that were easily comparable with each other as having the same structure.

Author: Emanuele Pesce (emanuelepesce.ep@gmail.com) <br/>
Collaborators: Angela Serra (angela.serra89@gmail.com) and Paola Galdi (paola.galdi@gmail.com) <br/>
The code is provided as is, for documentation purposes.  <br/>

Reference:
  > Serra, A., Galdi, P., Pesce, E., Fratello, M., Trojsi, F., Tedeschi, G., Tagliaferri, R. & Esposito, F. Strong-weak pruning for brain network identification in connectome-wide neuroimaging: Application to amyotrophic lateral sclerosis disease stage characterization. *International Journal of Neural Systems*, in press. Preprint: [ResearchGate](https://www.researchgate.net/publication/331131426_Strong-weak_pruning_for_brain_network_identification_in_connectome-wide_neuroimaging_Application_to_amyotrophic_lateral_sclerosis_disease_stage_characterization)

The following files are included:
* <b>data/</b> input data to launch the code and reproduce the results presented in the paper</br>
* <b>drivers/</b> R scripts to reproduce the results presented in the paper + an example script to run SWP on an user-defined network</br>
* <b>src/ </b> Source code for SWP and for producing a network mask based on Borda voting</br>


<b>How to run the code:</b><br/>

Required libraries: igraph, TopKLists<br/>
<br/>
In R, change directory to the <b>drivers/</b> folder and run:<br/>
```R
source('driver_SWP_paper.R')
```
or 
```R
source('example_SWP_algo.R')
```
Outputs are saved in ```data/other/borda```
