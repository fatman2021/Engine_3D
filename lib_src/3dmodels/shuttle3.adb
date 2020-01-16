--  Space shuttle 3D model. Copyright (c) Gautier de Montmollin 1999-2000
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use the herein contained 3D model for any purpose,
--  provided this copyright note remains attached and unmodified.

with Graphics.Colors;
with Screen;                              use Screen;
with Screen.Effects;                      use Screen.Effects;

package body Shuttle3 is
   use Graphics.Colors;

  Procedure Init is

    use Engine_3D;

    points: constant array( 1..305, 1..3 ) of float :=
      (  1=> (  3.5, -0.9, 0.9),  2=> (  3.5,  0.9, 0.9), -- caisse
         3=> ( 10.4, -0.9, 1.4),  4=> ( 10.4,  0.9, 1.4),
         5=> (  3.6, -0.9, 1.4),  6=> (  3.6,  0.9, 1.4),
         7=> (  6.5, -0.9, 0.9),  8=> (  6.5,  0.9, 0.9),
       181=> (  6.5, -0.9, 1.4),180=> (  6.5,  0.9, 1.4),
         9=> ( 11.5, -0.9, 0.6), 10=> ( 11.5,  0.9, 0.6),
        11=> ( 11.5, -0.9, 1.3), 12=> ( 11.5,  0.9, 1.3),
        13=> (  3.3, -1.1, 0.5), 14=> (  3.3,  1.1, 0.5), -- caisse react.
        15=> (  3.3, -1.1, 1.4), 16=> (  3.3,  1.1, 1.4),
        17=> (  1.9, -1.1, 1.4), 18=> (  1.9,  1.1, 1.4),
        19=> (  1.7, -1.1, 0.5), 20=> (  1.7,  1.1, 0.5),
        21=> (  3.6, -0.88, 1.7), 22=> (  3.6,  0.88, 1.7), -- battants
        23=> ( 10.4, -0.88, 1.7), 24=> ( 10.4,  0.88, 1.7),
        25=> (  3.6, -0.80, 1.9), 26=> (  3.6,  0.80, 1.9),
        27=> ( 10.4, -0.80, 1.9), 28=> ( 10.4,  0.80, 1.9),
        29=> (  3.6, -0.55, 2.1), 30=> (  3.6,  0.55, 2.1),
        31=> ( 10.4, -0.55, 2.1), 32=> ( 10.4,  0.55, 2.1),
        33=> (  3.6, -0.30, 2.2), 34=> (  3.6,  0.30, 2.2),
        35=> ( 10.4, -0.30, 2.2), 36=> ( 10.4,  0.30, 2.2),
        37=> (  3.6, 0.0, 2.24), 38=> ( 10.4, 0.0, 2.24),   -- sommet
        39=> (11.40, -0.61, 1.72), 40=> (11.40, 0.61, 1.72), -- cockpit
        41=> (11.00, -0.59, 1.82), 42=> (11.00, 0.59, 1.82),
        43=> (11.15, -0.50, 2.00), 44=> (11.15, 0.50, 2.00),
        45=> (11.15, -0.20, 2.20), 46=> (11.15, 0.20, 2.20),
        47=> (11.5, -0.29, 2.02), 48=> (11.5, 0.29, 2.02),
        49=> (11.65, -0.45, 1.8), 50=> (11.65, 0.45, 1.8),

        51=> (12.38,-0.70,1.13), 53=> (12.38,-0.50,1.15), --nez blanc dr
        55=> (12.38,-0.40,1.30), 57=> (12.38,-0.20,1.35),
        59=> (12.90,-0.40,1.00), 61=> (12.85,-0.25,1.20),
        52=> (12.38, 0.70,1.13), 54=> (12.38, 0.50,1.15), --nez blanc ga
        56=> (12.38, 0.40,1.30), 58=> (12.38, 0.20,1.35),
        60=> (12.90, 0.40,1.00), 62=> (12.85, 0.25,1.20),
        63=> (11.77,0.0,1.8), 64=> (12.38,0.0,1.5), -- arete du nez
        65=> (12.80,0.0,1.3),

        67=>(1.60,-1.10,1.39),69=>(2.40,-1.10,1.39), -- envel. react. ext. dr
        71=>(1.25,-1.20,1.70),73=>(2.60,-1.20,1.70),
        75=>(1.0,-1.40,1.88),77=>(1.20,-1.40,1.90),79=>(2.63,-1.40,1.90),
        81=>(1.0,-1.40,2.10),83=>(2.68,-1.40,2.10),
        85=>(1.25,-1.30,2.25),87=>(1.60,-1.30,2.25),89=>(2.66,-1.30,2.25),
        91=>(1.60,-1.25,2.4),93=>(2.70,-1.25,2.4),
        95=>(1.60,-1.05,2.7),97=>(2.85,-0.85,2.65),
        99=>(1.60,-0.70,2.8),101=>(2.95,-0.40,2.7),

        66=>(1.60, 1.10,1.39),68=>(2.40, 1.10,1.39), -- envel. react. ext. ga
        70=>(1.25, 1.20,1.70),72=>(2.60, 1.20,1.70),
        74=>(1.0, 1.40,1.88),76=>(1.20, 1.40,1.90),78=>(2.63, 1.40,1.90),
        80=>(1.0, 1.40,2.10),82=>(2.68, 1.40,2.10),
        84=>(1.25, 1.30,2.25),86=>(1.60, 1.30,2.25),88=>(2.66, 1.30,2.25),
        90=>(1.60, 1.25,2.4),92=>(2.70, 1.25,2.4),
        94=>(1.60, 1.05,2.7),96=>(2.85, 0.85,2.65),
        98=>(1.60, 0.70,2.8),100=>(2.95, 0.40,2.7),

        102=>(1.35,0.0,2.75), -- pte arr basse queue
        103=>(1.6,-0.155,2.4),105=>(2.0,-0.155,2.4),107=>(3.49,-0.07,2.3),
        104=>(1.6, 0.155,2.4),106=>(2.0, 0.155,2.4),108=>(3.49,+0.07,2.3),
        109=>(3.49,-0.07,2.6),111=>(1.2,-0.07,4.95),113=>(0.8,-0.155,4.75),--em.
        115=>(0.7,-0.155,5.0),
        110=>(3.49,+0.07,2.6),112=>(1.2,+0.07,4.95),114=>(0.8, 0.155,4.75),--em.
        116=>(0.7, 0.155,5.0),
        117=>(0.2,0.0,4.75),118=>(0.1,0.0,5.0), -- pte arr hte queue
        119=>(2.3,-0.155,2.75),120=>(2.3,+0.155,2.75),
        121=>(1.87,-0.12,2.9), 122=>(1.87,+0.12,2.9), -- empennage queue
        123=>(0.67,-0.12,4.65),124=>(0.67,+0.12,4.65),
        125=>(1.15,0.0,2.9), 126=>(0.32,0.0,4.65),

        127=>(2.9,-1.1,0.43),129=>(6.5,-1.1,0.8), --  aile blanc droite
        131=>(6.8,-1.52,0.7),133=>(6.35,-1.6,0.7),135=>(5.72,-1.9,0.6),
        137=>(2.9,-3.1,0.35),139=>(3.3,-3.1,0.42),141=>(4.61,-3.1,0.55),
        143=>(3.51,-4.2,0.31),145=>(3.3,-4.32,0.27),147=>(2.9,-4.38,0.25),
        149=>(4.61,-1.1,0.66),151=>(5.72,-1.1,0.66),

        128=>(2.9,+1.1,0.43),130=>(6.5,+1.1,0.8), --  aile blanc gauche
        132=>(6.8,+1.52,0.7),134=>(6.35,+1.6,0.7),136=>(5.72,+1.9,0.6),
        138=>(2.9,+3.1,0.35),140=>(3.3,+3.1,0.42),142=>(4.61,+3.1,0.55),
        144=>(3.51,+4.2,0.31),146=>(3.3,+4.32,0.27),148=>(2.9,+4.38,0.25),
        150=>(4.61,+1.1,0.66),152=>(5.72,+1.1,0.66),

        153=>(2.6,-4.61,0.20),155=>(2.55,-3.1,0.25), -- empennages aile droite
        157=>(1.98,-3.1,0.05),159=>(2.3,-4.65,0.05),
        161=>(2.55,-3.0,0.25),163=>(2.5,-1.1,0.25),
        165=>(1.70,-1.2,0.05), 167=>(1.96,-3.0,0.05),

        154=>(2.6,+4.61,0.20),156=>(2.55,+3.1,0.25), -- empennages aile gauche
        158=>(1.98,+3.1,0.05),160=>(2.3,+4.65,0.05),
        162=>(2.55,+3.0,0.25),164=>(2.5,+1.1,0.25),
        166=>(1.70,+1.2,0.05), 168=>(1.96,+3.0,0.05),

        169=>(0.9,0.0,5.1),170=>(1.3,0.0,5.0),171=>(3.6,0.0,2.7), -- boucl q

        173=>(1.55,-1.1,0.4),175=>(0.9,-1.0,0.3), -- caisse react dr
        172=>(1.55,+1.1,0.4),174=>(0.9,+1.0,0.3), -- caisse react ga
--         177=>(1.9,-1.1,0.2),179=>(3.3,-1.1,0.2),
--         176=>(1.9,+1.1,0.2),178=>(3.3,+1.1,0.2),

        182=>(13.36, 0.00,0.9), -- bouclier aile
        183=>(13.30,-0.25,0.8),184=>(13.30,+0.25,0.8),
        185=>(13.00,-0.40,0.75),186=>(13.00,+0.40,0.75),
        187=>(11.90,-0.88,0.50),188=>(11.90,+0.88,0.50),
        189=>( 8.40,-1.35,0.40),190=>( 8.40,+1.35,0.40),
        191=>( 6.15,-1.9,0.40),192=>( 6.15,+1.9,0.40),
        193=>( 5.40,-2.6,0.35),194=>( 5.40,+2.6,0.35),
        195=>( 4.25,-3.75,0.28),196=>( 4.25,+3.75,0.28),
        197=>( 3.75,-4.20,0.20),198=>( 3.75,+4.20,0.20),

        199=>(13.00,-0.3,0.50),200=>(13.00,+0.3,0.50), -- boucl. dessous
        201=>( 6.80,-1.4,0.10),202=>( 6.80,+1.4,0.10),
        203=>( 4.61,-3.1,0.00),204=>( 4.61,+3.1,0.00),
        205=>( 2.60,-4.61,0.00),206=>( 2.60,+4.61,0.00),
        209=>( 2.55,-3.10,0.00),210=>( 2.55,+3.10,0.00),
        211=>( 2.55,-3.00,0.00),212=>( 2.55,+3.00,0.00),
        213=>( 2.50,-1.10,0.00),214=>( 2.50,+1.10,0.00),
        215=>( 3.10,-1.10,0.05),216=>( 3.10,+1.10,0.05),

        217=>(12.9,0.0,0.4),218=>(6.8,0.0,0.0), -- centre, dessous
        219=>(4.61,0.0,0.0),220=>(3.1,0.0,0.0),
        177=>(8.6,-1.2,0.1),179=>(10.8,-0.75,0.07),
        176=>(8.6,+1.2,0.1),178=>(10.8,+0.75,0.07),

        207=>(13.15,0.0,1.16),208=>(13.15,0.0,0.6), -- nez, centre (182)

        -- Partie reacteurs --
-- Tuyeres inf. (B)ase
221=>(1.7950,-0.9000,1.1000),222=>(1.7950,0.9000,1.1000),
223=>(1.8692,-0.8121,1.3121),224=>(1.8692,0.8121,1.3121),
225=>(1.9000,-0.6000,1.4000),226=>(1.9000,0.6000,1.4000),
227=>(1.8692,-0.3879,1.3121),228=>(1.8692,0.3879,1.3121),
229=>(1.7950,-0.3000,1.1000),230=>(1.7950,0.3000,1.1000),
231=>(1.7208,-0.3879,0.8879),232=>(1.7208,0.3879,0.8879),
233=>(1.6900,-0.6000,0.8000),234=>(1.6900,0.6000,0.8000),
235=>(1.7208,-0.8121,0.8879),236=>(1.7208,0.8121,0.8879),
-- Tuyeres inf. (C)entre
237=>(1.1100,-0.6000,1.1000),238=>(1.1100,0.6000,1.1000),
-- Tuyeres inf. (E)xterieur
239=>(1.0450,-1.0000,1.2000),240=>(1.0450,1.0000,1.2000),
241=>(1.1440,-0.8828,1.4828),242=>(1.1440,0.8828,1.4828),
243=>(1.1850,-0.6000,1.6000),244=>(1.1850,0.6000,1.6000),
245=>(1.1440,-0.3172,1.4828),246=>(1.1440,0.3172,1.4828),
247=>(1.0450,-0.2000,1.2000),248=>(1.0450,0.2000,1.2000),
249=>(0.9460,-0.3172,0.9172),250=>(0.9460,0.3172,0.9172),
251=>(0.9050,-0.6000,0.8000),252=>(0.9050,0.6000,0.8000),
253=>(0.9460,-0.8828,0.9172),254=>(0.9460,0.8828,0.9172),
-- Tuyeres d'appoint (B)ase
255=>(1.9000,-0.9700,2.2000),256=>(1.9000,0.9700,2.2000),
257=>(1.9000,-0.9261,2.3061),258=>(1.9000,0.9261,2.3061),
259=>(1.9000,-0.8200,2.3500),260=>(1.9000,0.8200,2.3500),
261=>(1.9000,-0.7139,2.3061),262=>(1.9000,0.7139,2.3061),
263=>(1.9000,-0.6700,2.2000),264=>(1.9000,0.6700,2.2000),
265=>(1.9000,-0.7139,2.0939),266=>(1.9000,0.7139,2.0939),
267=>(1.9000,-0.8200,2.0500),268=>(1.9000,0.8200,2.0500),
269=>(1.9000,-0.9261,2.0939),270=>(1.9000,0.9261,2.0939),
-- Tuyeres d'appoint (C)entre
271=>(1.5700,-0.8700,2.2000),272=>(1.5700,0.8700,2.2000),
-- Tuyeres d'appoint (E)xterieur
273=>(1.4700,-1.1000,2.2000),274=>(1.4700,1.1000,2.2000),
275=>(1.5195,-1.0414,2.3414),276=>(1.5195,1.0414,2.3414),
277=>(1.5400,-0.9000,2.4000),278=>(1.5400,0.9000,2.4000),
279=>(1.5195,-0.7586,2.3414),280=>(1.5195,0.7586,2.3414),
281=>(1.4700,-0.7000,2.2000),282=>(1.4700,0.7000,2.2000),
283=>(1.4205,-0.7586,2.0586),284=>(1.4205,0.7586,2.0586),
285=>(1.4000,-0.9000,2.0000),286=>(1.4000,0.9000,2.0000),
287=>(1.4205,-1.0414,2.0586),288=>(1.4205,1.0414,2.0586),
-- Tuyere centrale (B)ase
289=>(1.9000,-0.3000,2.0000),290=>(1.9000,0.3000,2.0000),
293=>(1.9000,-0.2121,2.2121),294=>(1.9000,0.2121,2.2121),
304=>(1.9000,0.0000,2.3000),
301=>(1.9000,-0.0000,1.7000),
297=>(1.9000,-0.2121,1.7879),298=>(1.9000,0.2121,1.7879),
-- Tuyere centrale (C)entre
303=>(1.4600,0.0000,2.1000),
-- Tuyere centrale (E)xterieur
291=>(1.3600,-0.4000,2.1000),292=>(1.3600,0.4000,2.1000),
295=>(1.4590,-0.2828,2.3828),296=>(1.4590,0.2828,2.3828),
305=>(1.5000,0.0000,2.5000),
302=>(1.2200,-0.0000,1.7000),
299=>(1.2610,-0.2828,1.8172),300=>(1.2610,0.2828,1.8172)      );

    polys: constant array( 1..527, 1..4 ) of natural :=
      (  1=> (1,7,181,5),  2=> (8,2,6,180),   -- caisse
       213=> (7,3,181,0), 212=> (180,4,8,0),
         3=> (7,9,11,3), 4=> (8,4,12,10),
         5=> (13,15,17,19), 6=> (14,20,18,16), -- caisse react. (bl)
         7=> (1,5,15,13), 8=> (6,2,14,16), -- raccord caisses
--         9=>  (5,3,23,21),   10=> (4,6,22,24),   -- battants
         9=> (0,181,23,21),   10=> (22,24,180,0),   -- battant 1/3
       511=> (21,5,181,0), 512=> (180,6,22,0),
       513=> (23,181,3,0), 514=> (4,180,24,0),
         11=> (21,23,27,25), 12=> (24,22,26,28), -- battants
         13=> (25,27,31,29), 14=> (28,26,30,32), -- battants
         15=> (29,31,35,33), 16=> (32,30,34,36), -- battants
         17=> (33,35,38,37), 18=> (36,34,37,38), -- battants et sommet
         19=> ( 3,11,39,0), 20=> (40,12,4,0), -- cockpit, bas vers haut
         21=> ( 3,39,23,0), 22=> (24,40,4,0),
         23=> (23,39,41,0), 24=> (42,40,24,0),
         25=> (23,41,27,0), 26=> (28,42,24,0),
         27=> (27,41,43,0), 28=> (44,42,28,0),
         29=> (27,43,31,0), 30=> (32,44,28,0),
         31=> (31,43,45,0), 32=> (46,44,32,0),
         33=> (31,45,35,0), 34=> (36,46,32,0),
         35=> (35,45,0,38), 36=> (38,0,46,36), -- << hublots toit
         37=> (43,47,45,0), 38=> (46,48,44,0),
         39=> (38,45,46,0), 40=> (45,47,48,46), -- << sommet cockpit
         41=> (11,51,39,0), 42=> (40,52,12,0), -- nez blanc pres cockpit
         43=> (51,53,39,0), 44=> (40,54,52,0),
         45=> (39,53,49,0), 46=> (50,54,40,0),
         47=> (49,53,55,0), 48=> (56,54,50,0),
         49=> (49,55,63,0), 50=> (63,56,50,0),
         51=> (55,57,63,0), 52=> (63,58,56,0),
         53=> (57,64,63,0), 54=> (63,64,58,0),
         55=> (51,59,53,0), 56=> (54,60,52,0), -- nez blanc loin cockpit
         57=> (53,59,55,0), 58=> (56,60,54,0),
         59=> (55,59,57,0), 60=> (58,60,56,0),
         61=> (57,59,61,0), 62=> (62,60,58,0),
         63=> (57,61,64,0), 64=> (64,62,58,0),
         65=> (61,65,64,0), 66=> (64,65,62,0),
         67=>(67,69,73,71),68=>(70,72,68,66), -- envel. react.
         71=>(69,15,21,0),72=>(22,16,68,0),
         103=>(15,5,21,0),104=>(22,6,16,0),
         69=>(69,21,73,0),70=>(72,22,68,0),
         73=>(71,73,79,77),74=>(76,78,72,70),
         75=>(73,21,79,0),76=>(78,22,72,0),
         77=>(79,21,25,0),78=>(26,22,78,0),
         79=>(75,79,83,81),80=>(80,82,78,74),
         81=>(79,25,83,0),82=>(82,26,78,0),
         83=>(83,25,29,0),84=>(30,26,82,0),
         85=>(81,83,89,85),86=>(84,88,82,80),
         87=>(83,29,89,0),88=>(88,30,82,0),
         89=>(87,89,93,91),90=>(90,92,88,86),
         91=>(89,29,93,0),92=>(92,30,88,0),
         93=>(93,29,33,0),94=>(34,30,92,0),
         95=>(91,93,95,0),96=>(94,92,90,0),
         105=>(93,97,95,0),106=>(94,96,92,0),
         97=>(93,33,97,0),98=>(96,34,92,0),
         99=>(95,97,99,0),100=>(98,96,94,0),
         107=>(97,101,99,0),108=>(98,100,96,0),
         101=>(97,33,101,0),102=>(100,34,96,0),
         113=>(99,105,103,0),115=>(99,101,105,0), -- plateau dr
         117=>(101,107,105,0),109=>(101,33,107,0),111=>(33,37,107,0),
         114=>(104,106,98,0),116=>(106,100,98,0), -- plateau ga
         118=>(106,108,100,0),110=>(108,34,100,0),112=>(108,37,34,0),
         119=>(105,107,109,0),120=>(110,108,106,0), -- queue
         121=>(105,109,119,0),122=>(120,110,106,0),
         123=>(105,119,102,0),124=>(102,120,106,0),
         125=>(119,109,111,0),126=>(112,110,120,0),
         127=>(119,111,113,0),128=>(114,112,120,0),
         129=>(113,111,115,0),130=>(116,112,114,0),
         131=>(117,113,115,0),132=>(116,114,117,0),
         133=>(117,115,118,0),134=>(118,116,117,0),
         135=>(121,123,126,125),136=>(122,125,126,124), -- empennage queue bl

         137=>(127,137,139,13),139=>(137,147,145,139), -- aile droite (blanc)
         141=>(139,145,143,0),143=>(139,143,141,0),
         145=>(0,139,141,149),147=>(13,139,0,149), -- logo aile droite !
         149=>(149,141,135,0),151=>(149,135,151,0),153=>(151,135,133,0),
         155=>(151,133,129,0),157=>(133,131,129,0),159=>(129,131,9,0),
         161=>(7,129,9,0),
         -- 163=>(13,129,7,0),165=>(1,13,7,0),
         163=>(13,149,1,0),165=>(1,149,151,7),509=>(151,129,7,0),

         138=>(14,140,138,128),140=>(140,146,148,138), -- aile gauche (blanc)
         142=>(144,146,140,0),144=>(142,144,140,0),
         146=>(140,0,150,142),148=>(140,14,150,0), -- logo aile gauche !
         150=>(136,142,150,0),152=>(152,136,150,0),154=>(134,136,152,0),
         156=>(130,134,152,0),158=>(130,132,134,0),160=>(10,132,130,0),
         162=>(10,130,8,0),
         -- 164=>(8,130,14,0),166=>(8,14,2,0),
         164=>(2,150,14,0),166=>(8,152,150,2),510=>(8,130,152,0),

         167=>(159,153,155,0),169=>(159,155,157,0), -- empennage aile droite
         171=>(167,161,163,0),173=>(167,163,165,0),
         168=>(156,154,160,0),170=>(158,156,160,0), -- empennage aile gauche
         172=>(164,162,168,0),174=>(166,164,168,0),

         175=>(41,39,43,0),176=>(39,47,43,0),177=>(39,49,47,0), -- vitre co.
         178=>(47,49,63,0),179=>(47,63,48,0),180=>(48,63,50,0),
         181=>(48,50,40,0),182=>(44,48,40,0),183=>(44,40,42,0),

         -- Now the dark side...

         184=>(118,115,169,0),185=>(169,115,170,0), -- bouclier queue dr
         186=>(115,111,170,0),187=>(111,109,170,0),
         188=>(170,109,171,0),189=>(107,37,171,109),
         190=>(169,116,118,0),191=>(170,116,169,0), -- bouclier queue ga
         192=>(170,112,116,0),193=>(170,110,112,0),
         194=>(171,110,170,0),195=>(110,171,37,108),

         -- arriere empennage & cache
         196=>(126,123,124,0),197=>(121,122,124,123),198=>(125,122,121,0),
         199=>(117,114,113,0),200=>(114,120,119,113),201=>(102,119,120,0),

         203=>(173,172,174,175), -- caisse react (202 rempl.)
         204=>(175,174,214,213), -- 205 r.
         206=>(17,173,19,0),207=>(173,175,213,0),208=>(19,163,13,0),
         209=>(20,172,18,0),210=>(214,174,172,0),211=>(14,164,20,0),
         205=>(163,19,173,213),516=>(214,172,20,164),

         215=>(9,51,11,0),214=>(12,52,10,0), -- bouclier
         217=>(163,161,127,0),216=>(128,162,164,0),
         219=>(161,137,127,0),218=>(128,138,162,0),
         221=>(161,155,137,0),220=>(138,156,162,0),
         223=>(153,137,155,0),222=>(156,138,154,0),
         225=>(153,147,137,0),224=>(138,148,154,0),

         227=>(153,197,147,0),228=>(148,198,154,0), -- bouclier aile
         229=>(147,197,145,0),230=>(146,198,148,0),
         231=>(197,143,145,0),232=>(146,144,198,0),
         233=>(197,195,143,0),234=>(144,196,198,0),
         235=>(143,195,141,0),236=>(142,196,144,0),
         237=>(195,193,141,0),238=>(142,194,196,0),
         239=>(141,193,135,0),240=>(136,194,142,0),
         241=>(193,191,135,0),242=>(136,192,194,0),
         243=>(135,191,133,0),244=>(134,192,136,0),
         245=>(191,131,133,0),246=>(134,132,192,0),
         247=>(191,189,131,0),248=>(132,190,192,0),
         249=>(131,189,  9,0),250=>( 10,190,132,0),
         251=>(189,187,  9,0),252=>( 10,188,190,0),
         253=>(  9,187, 51,0),254=>( 52,188, 10,0),
         255=>(187,185, 51,0),256=>( 52,186,188,0),
         257=>(185, 59, 51,0),258=>( 52, 60,186,0),
         259=>(185,183, 59,0),260=>( 60,184,186,0),

         -- Nez (de bas en haut)

         261=>(199,217,208,0),262=>(208,217,200,0),
         263=>(183,199,208,0),264=>(208,200,184,0),
         265=>(183,208,182,0),266=>(182,208,184,0),
         267=>( 59,183,207,0),268=>(207,184, 60,0),
         269=>(183,182,207,0),270=>(207,182,184,0),
         271=>( 61, 59,207,0),272=>(207, 60, 62,0),
         273=>( 61,207, 65,0),274=>( 65,207, 62,0),

         -- Dessous

         275=>(185,199,183,0),276=>(200,186,184,0),
         277=>(187,199,185,0),278=>(200,188,186,0),
--          279=>(201,199,187,0),280=>(188,200,202,0),
--          281=>(201,187,189,0),282=>(190,188,202,0),
--          283=>(201,218,199,0),284=>(200,218,202,0),
--          279=>(189,201,218,0),280=>(218,202,190,0),
--          281=>(187,189,218,0),282=>(218,190,188,0),
--          283=>(199,187,218,0),284=>(218,188,200,0),
         279=>(201,177,189,0),280=>(190,176,202,0),
         281=>(177,179,189,0),282=>(190,178,176,0),
         283=>(189,179,187,0),284=>(188,178,190,0),
         517=>(218,177,201,0),518=>(202,176,218,0),
         519=>(218,217,177,0),520=>(176,217,218,0),
         521=>(177,217,179,0),522=>(178,217,176,0),
         523=>(179,217,187,0),524=>(188,217,178,0),
         525=>(187,217,199,0),526=>(200,217,188,0),

         285=>(218,217,199,0),286=>(200,217,218,0),
         287=>(191,201,189,0),288=>(190,202,192,0),
         289=>(193,201,191,0),290=>(192,202,194,0),
         291=>(203,201,193,0),292=>(194,202,204,0),
         293=>(203,219,201,0),294=>(202,219,204,0),
         295=>(203,193,195,0),296=>(196,194,204,0),
         297=>(197,203,195,0),298=>(196,204,198,0),
         299=>(205,203,197,0),300=>(198,204,206,0),
         301=>(205,209,203,0),302=>(204,210,206,0),
         303=>(209,211,203,0),304=>(204,212,210,0),
         305=>(211,215,203,0),306=>(204,216,212,0),
         307=>(211,213,215,0),308=>(216,214,212,0),
         309=>(215,219,203,0),310=>(204,219,216,0),
         311=>(215,220,219,0),312=>(219,220,216,0),
         226=>(219,202,218,0),515=>(219,218,201,0),
         527=>(214,216,215,213),

         313=>(159,205,153,0),314=>(154,206,160,0),-- empennages ailes (noir)
         315=>(157,209,205,159),316=>(160,206,210,158), -- +/- plat...
         317=>(155,153,157,0),318=>(158,154,156,0),
         319=>(161,211,209,155),320=>(156,210,212,162),
         321=>(167,211,161,0),322=>(162,212,168,0),
         323=>(165,213,211,167),324=>(168,212,214,166), -- +/- plat...
         325=>(165,163,213,0),326=>(214,164,166,0),
         327=>(205,197,153,0),328=>(154,198,206,0),

         -- Reacteurs, plaque partie asym y=0

         329=>(235,233,173,0),330=>(172,234,236,0),
         331=>(235,173,17,221),332=>(222,18,172,236),
         333=>(221,17,223,0),334=>(224,18,222,0),
         335=>(223,17,225,0),336=>(226,18,224,0),
         337=>(301,225,17,297),338=>(298,18,226,301),
         339=>(297,17,267,0),340=>(268,18,298,0),
         341=>(267,17,71,0),342=>(70,18,268,0),
         343=>(17,67,71,0),344=>(70,66,18,0),
         345=>(267,71,269,0),346=>(270,70,268,0),
         347=>(269,71,77,0),348=>(76,70,270,0),
         349=>(269,77,81,0),350=>(80,76,270,0),
         351=>(77,75,81,0),352=>(80,74,76,0),
         353=>(269,81,255,0),354=>(256,80,270,0),
         355=>(255,81,85,0),356=>(84,80,256,0),
         357=>(255,85,91,0),358=>(90,84,256,0),
         359=>(255,91,257,0),360=>(258,90,256,0),
         361=>(257,91,95,0),362=>(94,90,258,0),
         363=>(257,95,259,0),364=>(260,94,258,0),
         365=>(259,95,99,0),366=>(98,94,260,0),
         367=>(99,261,259,0),368=>(260,262,98,0),
         369=>(103,261,99,0),370=>(98,262,104,0),
         371=>(103,261,263,0),372=>(264,262,104,0),
         373=>(103,293,263,0),374=>(264,294,104,0),
         375=>(105,293,103,0),376=>(104,294,106,0),
         377=>(303,293,105,0),378=>(106,294,304,0),
         379=>(293,289,265,263),380=>(264,266,290,294),
         381=>(297,267,265,289),382=>(290,266,268,298),

         -- Reacteurs, plaque partie sym y=0

         383=>(172,173,233,234),384=>(234,233,231,232),
         385=>(232,231,229,230),386=>(230,229,227,228),
         387=>(228,227,225,226),388=>(226,225,301,0),
         389=>(106,303,105,0),

         -- Tuyeres inf., ext.
         391=>(221,241,239,0),390=>(240,242,222,0),
         393=>(221,223,241,0),392=>(242,224,222,0),
         395=>(223,243,241,0),394=>(242,244,224,0),
         397=>(223,225,243,0),396=>(244,226,224,0),
         399=>(225,245,243,0),398=>(244,246,226,0),
         401=>(225,227,245,0),400=>(246,228,226,0),
         403=>(247,245,227,0),402=>(228,246,248,0),
         405=>(247,227,229,0),404=>(230,228,248,0),
         407=>(249,247,229,0),406=>(230,248,250,0),
         409=>(249,229,231,0),408=>(232,230,250,0),
         411=>(251,249,231,0),410=>(232,250,252,0),
         413=>(251,231,233,0),412=>(234,232,252,0),
         415=>(251,233,253,0),414=>(254,234,252,0),
         417=>(253,233,235,0),416=>(236,234,254,0),
         419=>(253,235,239,0),418=>(240,236,254,0),
         421=>(239,235,221,0),420=>(222,236,240,0),

         -- Tuyeres sup., ext.
         423=>(255,275,273,0),422=>(274,276,256,0),
         425=>(255,257,275,0),424=>(276,258,256,0),
         427=>(257,277,275,0),426=>(276,278,258,0),
         429=>(257,259,277,0),428=>(278,260,258,0),
         431=>(259,279,277,0),430=>(278,280,260,0),
         433=>(259,261,279,0),432=>(280,262,260,0),
         435=>(261,281,279,0),434=>(280,282,262,0),
         437=>(261,263,281,0),436=>(282,264,262,0),
         439=>(283,281,263,0),438=>(264,282,284,0),
         441=>(263,265,283,0),440=>(284,266,264,0),
         443=>(285,283,265,0),442=>(266,284,286,0),
         445=>(285,265,267,0),444=>(268,266,286,0),
         447=>(287,285,267,0),446=>(268,286,288,0),
         449=>(287,267,269,0),448=>(270,268,288,0),
         451=>(287,269,273,0),450=>(274,270,288,0),
         453=>(273,269,255,0),452=>(256,270,274,0),

         -- Tuyere centrale, ext.
         455=>(302,301,299,0),454=>(300,301,302,0),
         457=>(299,301,297,0),456=>(301,300,298,0),
         459=>(299,297,289,0),458=>(290,298,300,0),
         461=>(299,289,291,0),460=>(292,290,300,0),
         463=>(291,289,295,0),462=>(296,290,292,0),
         465=>(289,293,295,0),464=>(296,294,290,0),
         467=>(293,304,295,0),466=>(296,304,294,0),
         469=>(295,304,305,0),468=>(305,304,296,0),

         -- Tuyeres inf., int.
         471=>(239,241,237,0),470=>(238,242,240,0),
         473=>(241,243,237,0),472=>(238,244,242,0),
         475=>(243,245,237,0),474=>(238,246,244,0),
         477=>(245,247,237,0),476=>(238,248,246,0),
         479=>(247,249,237,0),478=>(238,250,248,0),
         481=>(249,251,237,0),480=>(238,252,250,0),
         483=>(251,253,237,0),482=>(238,254,252,0),
         485=>(253,239,237,0),484=>(238,240,254,0),

         -- Tuyeres sup., int.
         487=>(273,275,271,0),486=>(272,276,274,0),
         489=>(275,277,271,0),488=>(272,278,276,0),
         491=>(277,279,271,0),490=>(272,280,278,0),
         493=>(279,281,271,0),492=>(272,282,280,0),
         495=>(281,283,271,0),494=>(272,284,282,0),
         497=>(283,285,271,0),496=>(272,286,284,0),
         499=>(285,287,271,0),498=>(272,288,286,0),
         501=>(287,273,271,0),500=>(272,274,288,0),

         -- Tuyere centrale, int.
         503=>(302,299,303,0),502=>(303,300,302,0),
         505=>(299,291,303,0),504=>(303,292,300,0),
         507=>(291,295,303,0),506=>(303,296,292,0),
         202=>(295,305,303,0),508=>(303,305,296,0)

      );


    f: constant:= fl_screen_virtual_size * 0.057;

    int_min: constant intensity:= (50 * intensity'first) / 100;
    int_max: constant intensity:= (03 * intensity'first) / 100;
    cmin, cmax: Color_Index;

    begin
      Shuttle3_obj:=
        New Object_3D( Max_points=> points'length(1),
                       Max_faces => polys'length(1) );

      for i in points'range(1) loop
        Shuttle3_obj.baseobj(i):=
          ( integer((points(i,1) - 6.2) * f),
            integer(points(i,2) * f),
            integer(points(i,3) * f) );
      end loop;

      Shuttle3_obj.num_of_faces:= polys'length(1);
      for i in polys'range(1) loop
        case i is
         when 1..174 |
              212|213|509..514 => cmin:= 16; cmax:= 31; -- blanc
         when 175..183 => cmin:=  64; cmax:=  72; -- bleu vitres
         when 265|266|
              269|270  => cmin:=   2; cmax:=  18; -- gris (nez)
         when 227..246 |
              289..292 |
              295..300 => cmin:=  2; cmax:=  20; -- gris (bouclier aile)
         when 329..389 => cmin:=  0;  cmax:=  14; -- support reacteurs
         when 390..469 => cmin:=  0;  cmax:=  17; -- tuyeres reacteurs
         when 470..485|502..508 | 
              202      => cmin:= 217; cmax:= 239; -- feu reacteurs
         when 486..501 => cmin:= 217; cmax:= 231; -- feu reacteurs appoint
         when others   => cmin:=   0; cmax:=  15; -- noir
        end case;
        Shuttle3_obj.faces(i):=
         ((polys(i,1),polys(i,2),polys(i,3),polys(i,4)),
          false,  null, 1,1, int_min,int_max, cmin, cmax);
      end loop;

      Shuttle3_obj.center:= (0.0,0.0,1.0*fl_screen_virtual_size);
      Shuttle3_obj.id:= "Shuttle3_obj    ";
      Init_object(Shuttle3_obj.all);

    end Init;

end Shuttle3;
