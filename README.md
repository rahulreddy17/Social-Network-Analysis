# Social-Network-Analysis

The network was generated using email data from a large European research institu-
tion. We have anonymized information about all incoming and outgoing email between
members of the research institution. There is an edge (u, v) in the network if person
u sent person v at least one email. The e-mails only represent communication between
institution members (the core), and the dataset does not contain incoming messages
from or outgoing messages to the rest of the world.

The dataset also contains "ground-truth" community memberships of the nodes.
Each individual belongs to exactly one of 42 departments at the research institute.
This network represents the "core" of the email-EuAll network, which also contains
links between members of the institution and people outside of the institution (although
the node IDs are not the same).

# Data Sets
1) email-Eu-core.txt
2) email-Eu-core-department-labels.txt

# Objective
Built a shiny app using the above datasets and present all the insights and visualizations using the app. All computations has been performed as part of the R script and final insights are displayed in my shiny app. Different packages has been explored to understand the logic behind the computations of centrality and betweenness.
