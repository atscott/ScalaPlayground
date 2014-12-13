val defibStrings = List("1;Maison de la Prevention Sante;6 rue Maguelone 340000 Montpellier;;3,87952263361082;43,6071285339217",
  "2;Hotel de Ville;1 place Georges Freche 34267 Montpellier;;3,89652239197876;43,5987299452849",
  "3;Zoo de Lunaret;50 avenue Agropolis 34090 Mtp;;3,87388031141133;43,6395872778854")

val defibrillators = for (d <- defibStrings) yield {
  (d split ";").toList
}

val defibrillators2 = for (d <- completeFile) yield {
  (d split ";").toList
}

findClosestDefibrillator(defibrillators, 43, 3.833542)
findClosestDefibrillator(defibrillators2, 43.634646, 3.833542)


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
def findClosestDefibrillator(defibrillators: List[List[String]], myLat: Double, myLong: Double): List[String] = {
  defibrillators.minBy(m => distance(getLatitude(m), getLongitude(m), myLat, myLong))
}
def distance(lat1: Double, long1: Double, lat2: Double, long2: Double): Double = {
  val dlon = long2 - long1
  val dlat = lat2 - lat1
  val a = math.pow(math.sin(dlat / 2), 2) + math.cos(lat1) * math.cos(lat2) * math.pow(math.sin(dlon / 2), 2)
  val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
  6371 * c
}
def getLatitude(d: List[String]): Double = getCoordinate(5)(d)
def getLongitude(d: List[String]): Double = getCoordinate(4)(d)
def getCoordinate(i: Int)(d: List[String]): Double =
  d.drop(i).head.replace(',', '.').toDouble
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

val completeFile = List("1;Maison de la Prevention Sante;6 rue Maguelone 340000 Montpellier;04 67 02 21 60;3,87952263361082;43,6071285339217",
  "2;Hotel de Ville;1 place Georges Freche 34267 Montpellier;04 67 34 44 93;3,89652239197876;43,5987299452849",
  "3;Zoo de Lunaret;50 avenue Agropolis 34090 Mtp;04 67 54 45 23;3,87388031141133;43,6395872778854",
  "4;Centre municipal Garosud;34000 Montpellier;04 67 34 74 62;3,85859221929501;43,5725732056683",
  "14;Service surveillance voie publique (ASVP); 8 Avenue Louis Blanc;04 99 58 80 31-32;3,87964814275905;43,6144971208687",
  "16;Poste de police Ecusson Centre ville;19 bis Rue durand 34000 Montpellier;04 67 34 70 89;3,87860749270054;43,6050174770208",
  "17;Unite Service Fourriere;1945 avenue de toulouse;04 67 06 10 51;3,85396082760103;43,5873825371736",
  "18;Poste de police Hotel de ville;789 chemin de moulares;;3,89399056177745;43,5988579879724",
  "20;Palais des sports Pierre-de-Coubertin;Avenue de Naples 34000 Montpellier;04 67 03 02 24;3,81388672389191;43,6382964524906",
  "21;Gymnase Francois Spinosi;Rue de la Cavalade 34000 Montpellier;04 67 15 90 35;3,91771560379933;43,5989740159529",
  "22;Plateau sportif de Grammont Terrain 9, 10, 11;Avenue Albert Einstein 34000 Montpellier;04 67 64 29 43;3,93528362675365;43,6141400501416",
  "23;Maison de la Democratie;16, rue de la Republique 34 Mtp;04 67 34 88 00;3,87908231371128;43,605322134559",
  "24;Gymnase Albert Batteux;150 rue Francois-Joseph-Gossec 34000 Montpellier;04 67 03 02 24;3,85685695958441;43,5740760521572",
  "25;Gymnase Bernard-Jouanique;Rue Jaques-Dalcroze 34080 Montpellier;04 67 54 63 99;3,84713719383276;43,6209657932612",
  "26;Stade Municipal Sabathe;Impasse Saint-Cleophas 34000 Montpellier;04 67 47 10 22;3,87069461025189;43,5979909515727",
  "27;Salle de Sports Alain Achille;1 place Marcel-Godechot 34000 Montpellier;04 67 15 90 35;3,86929208332712;43,6186500172655",
  "28;Gymnase Alain Le Hetet;237 route de Vauguieres 34 Montpellier;04 67 65 39 69;3,90623909083357;43,6056481110058",
  "29;Maison pour tous Albert Dubout;1071 avenue de la Justice de Castelnau 34090 Montpellier;04 67 02 68 58;3,88249943562772;43,6264526269792",
  "30;Maison pour tous Albert Camus;118 allee Maurice-Bonafos 34080 Montpellier;04 67 27 3341;3,86999152558735;43,5968636559527",
  "31;Mairie annexe de proximite Mosson;111 place de Tipasa Mas de la Paillade 34080 Montpellier;04 67 75 19 10;3,81644667226683;43,6280352697417",
  "32;Maison pour tous Francois Villon;Rue des Araucarias 34080 Montpellier;04 67 45 04 57;3,839535231275;43,6153516866351",
  "33;Maison pour tous Melina Mercouri;842 rue de la Vieille Poste 34 Montpellier;04 99 92 23 80;3,90822213186834;43,6134538722091",
  "35;Cimetiere Saint-lazare;2 rond-point du Souvenir Francais 34000 Montpellier;04 67 75 34 46;3,93613553570902;43,6154360341943",
  "36;Cimetiere Saint-Etienne;Avenue Albert Einstein 34000 Montpellier;04 99 52 87 35;3,88995587137398;43,6260090150577",
  "37;Piscine PITOT;40 Allee J.RAYMOND MONTPELLIER;04 67 52 58 89;3,870303933901;43,6123825678961",
  "38;Piscine A. NAKACHE;237 route de Vauguieres MONTPELLIER;04 67 22 57 05;3,90629916344486;43,6064087905768",
  "39;Piscine J. TARIS;67 rue L.Michel MONTPELLIER;04 67 79 03 11;3,90104518912559;43,6205269425253",
  "41;Piscine J. VIVES;1933 AVENUE DE Maurin MONTPELLIER;04 67 27 74 79;3,87258256366286;43,5878357304832",
  "40;Piscine M. SPILLIART;154 Rue C.DESMOULINS MONTPELLIER;04 67 42 00 92;3,8431426079266;43,5982457580602",
  "42;Piscine S. BERLIOUX;551 rue Metairie de Saysset MONTPELLIER;04 67 65 38 71;3,89523245626307;43,5904333774241",
  "43;Piscine NEPTUNE;Avenue de Heidelberg MONTPELLIER;04 67 75 34 93;3,89308118861399;43,6074454675728",
  "44;Piscine OLYMPIQUE ANTIGONE;195 avenue J.CARTIER MONTPELLIER;04 67 15 63 04;3,81486874802702;43,6203748477124",
  "45;STADE DE LA MOSSON;Avenue de Heidelberg MONTPELLIER; 04 67 75 74 16;3,81316555213326;43,6218734166524",
  "46;STADE Y. DU MANOIR;Avenue de la Vanniere MONTPELLIER;04 67 13 60 00;3,85003952312189;43,5936065771106",
  "47;PALAIS DES SPORTS R.BOUGNOL;1000, avenue du Val de Montferrand MONTPELLIER;04 67 52 76 14;3,87431554927809;43,6380433134334",
  "48;PATINOIRE VEGAPOLIS;Place de France MONTPELLIER;04 99 522 600;3,91489059571308;43,6029210639592",
  "49;CAP OMEGA;Rond point Benjamin Franklin MONTPELLIER;04 67 59 30 01;3,91427706121347;43,618609351242",
  "50;S.F.M.A.;Avenue Albert EINSTEIN MONTPELLIER;04 67 22 83 78;3,93573870254207;43,6156043738023",
  "51;MUSEE FABRE;13, rue Montpellieret 34000 Montpellier;04 67 14 83 00;3,88015202860524;43,6117202928099",
  "52;MEDIATHEQUE E.ZOLA;218 Bd de l'Aeroport International 34000 Montpellier;04 67 34 87 00;3,89315695628937;43,6084838193755",
  "53;DOMAINE DE LA PROVIDENCE;1784 Avenue de TOULOUSE MONTPELLIER;04 99 64 25 80;3,85389341433135;43,5886305406899",
  "54;RDC PAVILLON JUNON;50 Place Zeus MONTPELLIER;04 67 13 69 27;3,89083244140238;43,6081660748674",
  "55;RDC PAVILLON ZEUS;50 Place Zeus MONTPELLIER;04 67 13 69 27;3,89081696430097;43,6078670518814",
  "58;PAVILLON ZEUS (Salle Marianne);50 Place Zeus MONTPELLIER;04 67 13 69 27;3,89066621413495;43,6077521695801",
  "56;E.S.B.A.M.A.;130 , RUE YEHUDI MONTPELLIER;04 99 58 32 87;3,88438064294719;43,6169436571347",
  "57;MONTPELLIER DANCE;18 Rue Sainte Ursule MONTPELLIER;04 67 60 83 60;3,87836849830977;43,6139908031415",
  "59;CRR;14 Rue Eugene Lisbonne 34000 Montpellier;04 67 66 88 46;3,87409666178277;43,610433894746",
  "60;TAM;Parking comedie centre ville;04 67 07 63 79;3,88022104344314;43,6089439889165",
  "61;TAM;Parking Corum;04 67 07 63 79;3,88238703743398;43,6140271812289",
  "62;CCAS (Banque d'Acceuil);125 place Thermidor 34000 MONTPELLIER;04 99 52 77 53 06 14 09 40 21;3,89927443503256;43,6020241114814",
  "65;Universite Montpellier 1 UFR d'Economie (1er etage, a cote des bureaux de l'administration); Avenue Raymond Dugrand CS 79606 34960 MONTPELLIER Cedex 2;04 34 43 24 44;3,89956530126954;43,6037248716731",
  "66;Universite Montpellier 1 UFR AES (1er etage, en face de la salle des professeurs);Espace Richter Avenue Raymond Dugrand CS 59640 34960 MONTPELLIER Cedex 2;04 34 43 23 33;3,89991643490189;43,6031038225234",
  "64;Universite Montpellier 1 ISEM IPAG (Loge);Espace Richter Bat b Vendemiaire CS 19519 34960 MONTPELLIER Cedex 2;ISEM : 04 34 43 20 00 IPAG : 04 67 15 85 46;3,89904523736634;43,6042583430944",
  "63;Universite Montpellier 1 Services Mutualises Richter PC Securite;Espace Richter Bat E, BIU, MdE, Rue Vendemiaire CS 19519 34960 MONTPELLIER Cedex 2;;3,89848334164309;43,6041786988646",
  "67;Universite Montpellier 1 UFR Droit (accueil batiment1);39 rue de l'universite 34060 Montpellier Cedex 2;04 67 61 54 00;3,87717345678089;43,6140582253597",
  "68;Universite Montpellier 1 UFR Droit (accueil batiment2);;;3,87633518840488;43,6138609333465",
  "69;Universite Montpellier 1 UFR Medecine (Loge batiment Historique);2 rue Ecole de Medecine CS 59001 34060 MONTPELLIER Cedex 2;04 67 60 10 00;3,87354913287527;43,612951860071",
  "70;Universite Montpellier 1 UFR Medecine (Loge Institut biologie);;;3,87409700190794;43,6150688127327",
  "71;Universite Montpellier 1 UFR Odontologie (Hall Premier Etage Bat A);545, avenue du Professeur J.L Viala 34193 MONTPELLIER Cedex 5;04 67 10 44 70;3,82353507060243;43,6362497267334",
  "72;Universite Montpellier 1 UFR Pharmacie (LogeBat A);15, avenue Charles Flahault BP 14491 34093 MONTPELLIER Cedex 5;04 67 54 80 00;3,86189868456889;43,6232360922128",
  "73;Universite Montpellier 1 UFR Pharmacie (Galerie a cote de la pharmacie experimentale);;;3,86034021457447;43,6220496094564",
  "74;Universite Montpellier 1 UFR Staps (Batiment A);700, avenue du Pic Saint-Loup 34090 MONTPELLIER;04 67 41 57 00;3,8538650451542;43,640831368515",
  "75;Universite Montpellier 1 UFR Staps (P1);;;3,84875782514269;43,6393278958929",
  "76;Universite Montpellier 1 UFR Staps (Palais des Sports);;;3,84872346174789;43,6401970759746",
  "77;Universite Montpellier 2 PC Securite;;04 67 14 31 11;3,86080925535177;43,6320478884427",
  "79;Universite Montpellier 2 Secretariat IAE;place Eugene Bataillon 34095 MONTPELLIER CEDEX 5;04 67 14 31 11;3,86173924182999;43,6324310982988",
  "78;Universite Montpellier 2 Couloir de la presidence 1er etage;;04 67 14 31 11;3,86403637133178;43,6320955198071",
  "80;Universite Montpellier 2 Laboratoire AREVA;;04 67 14 31 11;3,86719128258212;43,6340354958767",
  "81;Universite Montpellier 2 SCOPPS;;04 67 14 31 11;3,86651938060978;43,6329423512668",
  "82;Universite Montpellier 2 Laboratoire L2C;;04 67 14 31 11;3,86500072175922;43,6325272513173",
  "86;Universite Montpellier 2 Centre Sportif Universitaire Piscine;;04 67 14 31 11;3,86372265447782;43,6347707699201",
  "85;Universite Montpellier 2 Centre Sportif Universitaire Gymnase;Rue Emile Jeanbreau;04 67 14 31 11;3,86411975731741;43,634431102342",
  "84;Universite Montpellier 2 HALL IEM;;04 67 14 31 11;3,86621601700653;43,6350134840932",
  "88;Universite paul valery montpellier 3 Loge Entree;Av. Val de montferrand 34199 MONTPELLIER;04 67 14 55 23;3,86987294089832;43,6310996420996",
  "89;Universite paul valery montpellier batiment Marc Bloch;Route de mende 34199 MONTPELLIER;04 67 14 55 23;3,86914576305111;43,6327403750896",
  "90;Universite Paul Valery montpellier 3 Site Saint Charles Loge Entree;Rue du Professeur Henri Serre 34080 MONTPELLIER Arret tram \" albert 1er\";04 67 14 55 23;3,87378574801668;43,6165624740146",
  "91;Montpellier Ecole National Superieure de Chimie;8, rue de l'ecole Normale 34000 MONTPELLIER;04 67 14 72 83;3,86759222312708;43,6208396831232",
  "92;Montpellier Ecole National Superieure de Chimie (Laboratoire);104, rue de la galera 34090 MONTPELLIER;04 67 14 72 83;3,83738323298412;43,6371761537967",
  "93;Montpellier Ecole National Superieure de Chimie;220 - 276 rue de la galera 34090 MONTPELLIER;04 67 14 72 83;3,83828534323613;43,6375710817093",
  "94;CROUS de MONTPELLIER Restaurants Universitaires Boutonnet;2, Rue Emile Duploye 34090 MONTPELLIER Cedex 01;04 67 63 52 06;3,86940780515415;43,6234756772261",
  "95;CROUS de MONTPELLIER Restaurants Universitaires Triolet;1061, av. Prof. Joseph Anglada 34090 MONTPELLIER;04 67 63 50 16;3,86018125270489;43,631191261367",
  "96;CROUS de MONTPELLIER Restaurants Universitaires Vert-Bois;205, rue de la Chenaie 34090 MONTPELLIER;04 67 63 66 45;3,87086361964785;43,6348289810456",
  "98;CROUS de MONTPELLIER Services Centraux;2, rue Monteil 34033 Montpellier; 04 67 41 50 08;3,87001135797271;43,6224491662391",
  "97;CROUS de MONTPELLIER Restaurants Universitaires Richter;80, rue Brumaire- 34000 Montpellier;04 67 15 84 47;3,89922794228039;43,6028938663275",
  "99;CREPS;2 Avenue Charles Flahault 34090 MONTPELLIER;;3,86618003917991;43,6191123089151",
  "100;Maison des sports (Sport Et Psychologie) Herault Sport;200 avenue du Pere Soulas 34090 MONTPELLIER;04 67 54 82 29;3,86583323268375;43,6183423905225",
  "101;Herault Sport;747 avenue des apothicaires Parc Euromedecine   34090 Montpellier;04 67 54 82 29;3,83559883662065;43,6416646402407",
  "103;Lycee Frederic Bazille;3224 route de Mende 34093 MONTPELLIER;04 67 63 89 87;3,8639010584102;43,6460180385688",
  "104;Lycee Jean Mermoz;717 avenue Jean Mermoz 34000 MONTPELLIER;04 67 20 60 00;3,89080547314588;43,610863473281",
  "106;Lycee Leonard de Vinci;Rue du Professeur Blayac 34085 MONTPELLIER cedex 4;04 67 10 40 10;3,82313209556008;43,6271809795402",
  "105;Lycee Jules Ferry;270 avenue de la colline 34070 MONTPELLIER;04 67 10 74 01;3,84226731025644;43,6052528635409",
  "107;Caisse Primaire d'Assurance Maladie;29 cours Gambetta 34000 MONTPELLIER;04 99 52 54 49;3,87110915929521;43,6065196099402",
  "108;Caisse Primaire d'Assurance Maladie;90 allee Almicare Calvetti 34000 Montpellier;04 99 52 54 49;3,82126953167633;43,6322018829039",
  "109;Caisse d'assurance retraite et de la Sante au travail;29 cours Gambetta 34000 MONTPELLIER;04 67 12 94 72;3,87064343057042;43,6068847626242",
  "110;Caisse d'assurance retraite et de la Sante au travail;Century 2 , 101 place pierre Duhem le millenaire 34000 MONTPELLIER;04 67 12 94 72;3,91465549573187;43,6068978500869",
  "111;Prefecture de l'Herault;34 Place des Martyrs de la resistance 34000 MONTPELLIER;04 67 61 60 45;3,87675679668135;43,6114960399587",
  "113;Cour d'appel;1 rue Foch 34000 MONTPELLIER;04 34 08 81 92;3,87282071734522;43,6112848970996",
  "112;Tribunal de grande instance;Place Pierre Flotte 34000 MONTPELLIER;04 67 12 61 09;3,86914794017784;43,6102006063269",
  "115;Gare Sncf de Montpellier St Roch;1, Place Auguste Gibert 34000 MONTPELLIER;06 25 91 00 28;3,88084502925211;43,6047523852628",
  "114;Hotel de Police de Montpellier;206 avenue du Comte de Melgueil 34000 MONTPELLIER;04 99 13 50 00;3,89161633267666;43,603513899768",
  "116;Pharmacie de L'Europe;2600 avenue de l'europe 34080 MONTPELLIER;04 67 75 16 37;3,82007583943153;43,6418758605771",
  "117;Pharmacie de l'ovalie;2750 Boulevard Paul Valery 34070 MONTPELLIER;04 67 27 71 72;3,84964180769663;43,5950383978097",
  "118;Pharmacie Ravoire;33, Rue du Faubourg Saint JAUMES 34000 MONTPELLIER;04 67 63 38 84;3,86983030264785;43,6147553510548",
  "119;Citroen montpellier;730 Avenue des pres d'arenes 34000 MONTPELLIER;04 67 12 67 01;3,88299732333175;43,5906567856049",
  "120;Grand Garage de l'Herault Peugeot Montpellier (commerce);905 rue de l'industrie 34007 MONTPELLIER;04 67 06 25 02 04 67 06 25 25;3,88118576492958;43,5829591529706",
  "121;Grand Garage de l'Herault Peugeot Montpellier (atelier);905 rue de l'industrie 34007 MONTPELLIER;04 67 06 25 02 04 67 06 25 25;3,88146330454406;43,5835668089934",
  "122;Centre commercial Polygone PC Securite (es1 montpellier);1 rue des Pertuisanes 34000 MONTPELLIER;04 67 99 41 60;3,88578382216927;43,6083221486189",
  "123;Fnac Montpellier;Centre cial Le Polygone 1 rue des Pertuisanes 34000 MONTPELLIER;04 34 09 06 55;3,88567011518647;43,6085330470563",
  "124;Galeries La Fayette;Centre cial Le Polygone 1 rue des Pertuisanes BP 3521 34000 MONTPELLIER;04 67 64 83 00;3,88553285006607;43,6081092231254",
  "125;Geant casino Pres d'Arenes (Pc Securite); 504 Avenue du mas d'argelliers 34070 MONTPELLIER;04 67 86 43 69;3,88808523342942;43,586264441135",
  "126;Geant casino Celleneuve (Pc Securite);129 bis avenue de Lodeve 34070 MONTPELLIER;04 67 86 43 69 04 99 77 34 00;3,83992052015185;43,6125224275035",
  "127;Centre Commercial Odysseum;2 place de Lisbonne 34000 MONTPELLIER;04 67 13 50 55;3,92046106179072;43,6045260331335",
  "128;Magasin IKEA;Odysseum 34000 MONTPELLIER;;3,92438329923687;43,6041477817148",
  "129;Chronopost;1129 Rue de la castelle 34070 MONTPELLIER;04 67 99 11 03 06 69 58 35 62;3,86885461893472;43,5772303319782",
  "130;DELL;1 rond-point Benjamin Franklin 34000 MONTPELLIER;06 58 57 85 24;3,91169360147975;43,6184228864032",
  "131;France 3 Sud Montpellier;10 allee John Napier 34000 MONTPELLIER;04 67 20 30 40;3,90921459780798;43,6145658661223",
  "132;France Telecom;245 rue de la Galera 34000 MONTPELLIER;04 67 14 66 66;3,83704301955449;43,6384175720502",
  "133;Sanofi Aventis;371 rue professeur Blayac 34000 MONTPELLIER;04 99 77 78 79;3,82943569760855;43,6234283430937",
  "134;Veolia Eau;765 rue Henri Becquerel BP41246 34965 MONTPELLIER CEDEX 2;04 67 20 73 73 06 20 69 33 70;3,91517278210411;43,612096722739",
  "135;Banque de France;98 avenue de Lodeve 34061 MONTPELLIER;04 67 06 79 74;3,85350136943136;43,6102729619807",
  "136;Mutuelle des motards;1056 rue de la croix verte 34294 MONTPELLIER;04 67 72 73 20;3,84315528199888;43,642457500046",
  "137;Mutuelle des motards;1027 rue de la croix verte 34294 MONTPELLIER;04 67 72 73 20;3,84351021937092;43,6430362920199",
  "138;Groupama Sud;Place Jean Antoine de Chaptal 34000 MONTPELLIER;04 67 34 78 86;3,86748181412747;43,6031265793569",
  "139;Montpellier beton SERVANT Prestations;1, Rue de la Premiere Ecluse 34070 MONTPELLIER;04 67 92 15 10;3,89578183610751;43,5877632296267",
  "140;Hotel IBIS Centre Comedie;Allee Jules Milhau Immeuble le Triangle 34000 MONTPELLIER;04 99 13 29 99;3,88315927070696;43,6089881225671",
  "141;Hotel IBIS Montpellier Sud;164 avenue palavas 34070 MONTPELLIER;04 67 58 82 30;3,89172749729087;43,5892502551644",
  "142;Hotel mercure;Carrefour de l'aeroport 34000 MONTPELLIER;04 67 20 63 63;3,8940718175978;43,6089445631649",
  "143;Hotel mercure centre;Rue de la Spirale 34000 MONTPELLIER;04 67 99 89 89;3,88547541488289;43,6090902690373",
  "144;Hotel NOVOTEL;125 avenue Palavas 34070 MONTPELLIER;04 99 52 34 34;3,89234991103325;43,5895487658564",
  "145;CRS 56;1 Rue Louis Lepine 34000 MONTPELLIER;04 67 13 17 00;3,90653250762828;43,6122198808195",
  "146;Accueil clinique du millenaire Accueil;220 bd penelope 34000 MONTPELLIER;04 99 53 61 03;3,91375491076165;43,6020754291092",
  "147;Accueil clinique du millenaire Urgences;220 bd penelope 34000 MONTPELLIER;04 99 53 61 03;3,91353989374935;43,6014966033821",
  "148;Ametra;201 place de Thessalie 34000 MONTPELLIER;04 67 84 76 40;3,89226740440157;43,6075603632208",
  "149;apec;170 rue leon blum 34000 MONTPELLIER;;3,89052741878513;43,6087911504281",
  "150;Arcade SFGE;1-55 Rue de la Constituante 34000 MONTPELLIER;;3,89422323604739;43,6019066687056",
  "151;ASPM SECTION AFPS;1635 Avenue Albert Einstein 34000 MONTPELLIER;;3,9167804322775;43,6113532636814",
  "152;Cafeteria UFR AES;257-269 Rue Vendemiaire 34000 MONTPELLIER;;3,89935218745248;43,603025020351",
  "153;Caisse MSA Languedoc;4 place Jean Antoine de Chaptal 34000 MONTPELLIER;;3,86725032663434;43,6029957496972",
  "154;Centre Mutualiste Neurologique PROPARA;263 Rue du Caducee 34000 MONTPELLIER;;3,83221040954412;43,6433843496942",
  "155;Communaute d Agglomeration de Montpellier;50 place Zeus;;3,8906414844389;43,6076132052153",
  "156;D.D.S.I.S. HERAULT;2 Rue Duval-Jouve 34000 MONTPELLIER;;3,86542459122745;43,610311327355",
  "157;DDTM34 - site du Millenaire;233 rue Guglielmo Marconi 34000 MONTPELLIER;;3,91125189536332;43,6121330162831",
  "158;Ecole Superieure Des Beaux Arts;130 Rue Yehudi Menuhin 34000 MONTPELLIER;;3,88398298908056;43,6171934153182",
  "159;Faculte de Droit;Rue de l'Ecole Mage 34000 MONTPELLIER;;3,87687944359565;43,6137132951212",
  "160;faculte de pharmacie;15 avenue charles flahault 34000 MONTPELLIER;;3,86185840432793;43,6239506766346",
  "161;fafsea;2460 avenue albert einstein 34000 MONTPELLIER;;3,92771433730696;43,6106200898298",
  "162;Hotel des impots;40 rue de louvois 34000 MONTPELLIER;;3,81969688714667;43,6323423132911",
  "163;Inset de Montpellier;76 Place de la Revolution 34000 MONTPELLIER;;3,89825148899358;43,6039740268785",
  "164;maison agriculture;place chaptal 34000 MONTPELLIER;;3,86633327325595;43,6038860913024",
  "165;TALCO LR;40 rue de Pinville 34000 MONTPELLIER;;3,90506063238055;43,6138087474829",
  "166;Zenith Sud;avenue Albert Einstein 34000 MONTPELLIER;;3,93074702939992;43,6128228432964",
  "168;Action d'Urgence Internationale;1401 rue de fontcouverte 34070 MONTPELLIER;;3,85255897515535;43,5935288046927",
  "169;Cfpmea;501 des metairies de saysset 34070 MONTPELLIER;;3,89423707713714;43,5906087749725",
  "172;Amphitheatre d'O;121 Rue de la Carrierasse 34090 MONTPELLIER;;3,83507767297601;43,6351213340499",
  "173;Batiment k Iut montpellier 2;139 Avenue d'Occitanie 34090 MONTPELLIER;;3,85149578101552;43,6350358178596",
  "174;Bibliotheque st charles;Rue auguste broussonnet 34090 MONTPELLIER;;3,87258316660111;43,6155361363529",
  "175;Centre de formation professionnel croix rouge;Rue de la valesiere 34090 MONTPELLIER;;3,83009545801086;43,6415334891597",
  "176;Chru;Avenue Augustin Fliche 34090 MONTPELLIER;;3,86151292659671;43,6295976450315",
  "177;Chru euromed;Rue du caduce 34090 MONTPELLIER;;3,8350137442276;43,6425510639908",
  "179;CHU Lapeyronie hall d'accueil;Pont Lapeyronie 34090 MONTPELLIER;;3,85207580919409;43,6301375533552",
  "178;Chru lapeyronie;Avenue du doyen gaston giraud 34090 MONTPELLIER;;3,85081289792181;43,6313023033573",
  "181;Ipl sante envirronement durable Mediterrranee;778 rue de la croix verte 34090 MONTPELLIER;;3,84083465416705;43,6447155674701",
  "182;Les Jardins de Grasse;1482 Rue de Saint-Priest 34090 MONTPELLIER;;3,83534242743301;43,6375404910418",
  "183;Parcs Nationaux de France;1037 rue Jean Francois Breton 34090 MONTPELLIER;;3,87871736858804;43,6474178784164",
  "187;Cirad;Avenue agropolis 34398 MONTPELLIER;;3,868430789818;43,6504884118088",
  "188;Mornay;26 allee jules milhau 34965 MONTPELLIER;;3,88335468006384;43,6090204423773",
  "189;Boulodrome Bernard Gasset;122 avenue Maurice Planes 34070 MONTPELLIER;;3,84329169898554;43,5967806501323")
