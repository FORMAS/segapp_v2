VERB <- "(VB |VBD |VBG |VBN |VBP |VBZ )";
ADV <- "(RB |RBR |RBS |WRB )";
NOUN <- "(NN |NNS |NNP |NNPS )";
NOUN <- "(NN |NNS )";
PROPER_NOUN <- "(NNP |NNPS )";
ADJ <- "(JJ |JJR | JJS )";
PRON <- "(PRP |PRP\\$ |WP |WP\\$ )";
DET <- "(DT |PDT |WDT )";
PARTICLE <- "(RP )";
PREP <- "(IN |TO )";
INF_MARKER <- "(LS )";

V <- paste0("(", VERB, PARTICLE, "?", ADV, "?)")
W <- paste0("(", NOUN, "|", ADJ, "|", ADV, "|", PRON, "|", DET, ")");
P <- paste0("(", PREP, "|", PARTICLE, "|", INF_MARKER, ")");

VP <- paste0("(", V, P, ")");
VW_P = paste0("(", V, W, "*", P, ")");

PATTERN <- paste0("(", VW_P, "|", VP, "|", V, ")")

CHUNK <- "B-NP (I-NP )*(B-PP B-NP (I-NP )*)*"

#gregexpr(PATTERN, 'VBD PRP$ NN IN DT')
