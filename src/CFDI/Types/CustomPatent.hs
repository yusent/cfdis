module CFDI.Types.CustomPatent where

import CFDI.Types.Type
import Control.Error.Safe (justErr)
import Data.Set           (fromList, member)
import Text.Read          (readMaybe)

newtype CustomPatent = CustomPatent Int deriving (Eq, Show)

instance Type CustomPatent where
  parseExpr c = justErr NotInCatalog maybeCustomPatent
    where
      maybeCustomPatent = CustomPatent <$> (readMaybe c >>= isValid)
      isValid x
        | x `member` validCodes = Just x
        | otherwise = Nothing
      validCodes = fromList
        [ 0, 74, 101, 115, 122, 123, 149, 150, 182, 188, 203, 205, 227, 246
        , 247, 271, 273, 276, 289, 294, 309, 310, 312, 316, 318, 321, 323, 327
        , 328, 334, 339, 347, 348, 350, 361, 368, 381, 382, 387, 388, 389, 390
        , 394, 398, 400, 401, 403, 404, 406, 407, 411, 412, 413, 414, 418, 419
        , 420, 424, 425, 426, 427, 428, 429, 430, 432, 438, 440, 441, 442, 444
        , 445, 447, 449, 451, 454, 457, 460, 462, 464, 465, 467, 469, 472, 474
        , 476, 477, 478, 480, 481, 482, 483, 485, 486, 487, 489, 491, 492, 493
        , 494, 496, 498, 499, 500, 501, 503, 504, 506, 508, 509, 510, 512, 513
        , 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 531, 532, 534
        , 535, 537, 538, 539, 540, 544, 546, 547, 548, 549, 551, 552, 553, 554
        , 555, 556, 557, 558, 559, 562, 563, 565, 566, 569, 570, 571, 572, 573
        , 574, 575, 576, 578, 579, 580, 581, 582, 584, 586, 587, 588, 589, 590
        , 592, 595, 596, 597, 598, 599, 600, 601, 604, 606, 607, 608, 609, 610
        , 611, 613, 614, 615, 617, 619, 620, 622, 624, 625, 626, 627, 628, 629
        , 630, 631, 632, 633, 634, 636, 637, 638, 639, 640, 641, 643, 644, 645
        , 646, 647, 648, 649, 651, 652, 653, 654, 655, 656, 657, 659, 661, 662
        , 663, 664, 665, 666, 667, 668, 669, 670, 671, 672, 673, 674, 675, 676
        , 677, 678, 679, 680, 681, 682, 683, 684, 685, 686, 687, 688, 689, 690
        , 691, 692, 693, 695, 696, 697, 699, 700, 701, 702, 705, 706, 707, 708
        , 709, 710, 711, 712, 713, 714, 715, 716, 717, 718, 719, 720, 721, 722
        , 723, 724, 725, 726, 727, 728, 729, 731, 732, 733, 734, 735, 736, 737
        , 738, 739, 740, 741, 742, 743, 744, 745, 746, 747, 748, 749, 750, 751
        , 752, 753, 754, 755, 756, 757, 758, 760, 761, 762, 763, 764, 765, 766
        , 767, 768, 769, 770, 771, 772, 773, 774, 775, 776, 777, 778, 779, 780
        , 781, 782, 783, 784, 785, 786, 787, 788, 790, 792, 794, 795, 796, 797
        , 798, 799, 800, 801, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812
        , 813, 814, 815, 816, 817, 818, 819, 820, 821, 822, 823, 825, 828, 829
        , 830, 831, 832, 833, 834, 835, 836, 837, 838, 839, 840, 841, 842, 843
        , 844, 845, 846, 847, 848, 849, 850, 851, 852, 853, 854, 855, 856, 857
        , 858, 859, 860, 862, 863, 864, 867, 868, 869, 870, 871, 872, 873, 875
        , 876, 877, 878, 879, 880, 881, 882, 883, 884, 885, 886, 887, 888, 889
        , 890, 891, 892, 895, 896, 897, 898, 899, 900, 901, 902, 903, 904, 905
        , 906, 907, 908, 909, 910, 911, 912, 913, 914, 915, 916, 917, 918, 919
        , 920, 921, 922, 925, 926, 927, 928, 929, 930, 931, 932, 933, 934, 935
        , 936, 937, 938, 939, 940, 941, 942, 943, 944, 945, 946, 947, 948, 949
        , 950, 951, 952, 953, 954, 955, 956, 957, 958, 959, 960, 961, 962, 963
        , 964, 965, 966, 967, 968, 969, 970, 971, 972, 973, 974, 975, 976, 977
        , 978, 979, 980, 981, 982, 983, 984, 985, 986, 987, 988, 989, 990, 991
        , 992, 993, 994, 995, 997, 999, 1000, 1001, 1002, 1003, 1004, 1005
        , 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016
        , 1017, 1018, 1019, 1020, 1021, 1022, 1023, 1024, 1025, 1026, 1027
        , 1028, 1029, 1030, 1031, 1032, 1033, 1034, 1035, 1036, 1037, 1038
        , 1039, 1040, 1041, 1042, 1043, 1044, 1045, 1046, 1047, 1048, 1049
        , 1050, 1051, 1052, 1054, 1055, 1056, 1057, 1058, 1059, 1060, 1061
        , 1062, 1063, 1064, 1065, 1066, 1067, 1068, 1069, 1070, 1071, 1072
        , 1073, 1074, 1075, 1076, 1077, 1078, 1079, 1080, 1081, 1082, 1084
        , 1085, 1086, 1087, 1088, 1089, 1090, 1092, 1094, 1095, 1096, 1097
        , 1098, 1099, 1100, 1102, 1103, 1104, 1105, 1106, 1107, 1108, 1109
        , 1111, 1112, 1113, 1114, 1115, 1116, 1117, 1120, 1121, 1122, 1123
        , 1124, 1125, 1126, 1127, 1128, 1129, 1130, 1132, 1136, 1137, 1138
        , 1139, 1141, 1142, 1144, 1145, 1148, 1149, 1150, 1152, 1153, 1154
        , 1155, 1156, 1157, 1158, 1159, 1160, 1161, 1162, 1163, 1164, 1165
        , 1166, 1167, 1168, 1169, 1170, 1171, 1172, 1173, 1174, 1175, 1176
        , 1177, 1178, 1179, 1181, 1182, 1186, 1187, 1188, 1189, 1190, 1191
        , 1192, 1193, 1194, 1195, 1196, 1197, 1198, 1199, 1200, 1201, 1202
        , 1203, 1204, 1205, 1206, 1207, 1208, 1209, 1210, 1211, 1212, 1213
        , 1214, 1215, 1216, 1217, 1218, 1219, 1220, 1221, 1222, 1223, 1224
        , 1225, 1226, 1227, 1228, 1229, 1230, 1231, 1232, 1233, 1234, 1235
        , 1236, 1237, 1238, 1239, 1240, 1241, 1242, 1243, 1244, 1245, 1246
        , 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257
        , 1258, 1259, 1260, 1261, 1262, 1263, 1264, 1265, 1266, 1267, 1268
        , 1269, 1270, 1271, 1272, 1273, 1274, 1275, 1276, 1277, 1278, 1279
        , 1280, 1281, 1282, 1283, 1284, 1285, 1286, 1287, 1288, 1289, 1290
        , 1291, 1293, 1294, 1295, 1296, 1297, 1298, 1299, 1300, 1301, 1302
        , 1303, 1304, 1305, 1306, 1307, 1308, 1309, 1311, 1312, 1313, 1314
        , 1315, 1316, 1317, 1318, 1319, 1320, 1321, 1322, 1323, 1324, 1325
        , 1326, 1327, 1328, 1329, 1330, 1331, 1332, 1333, 1334, 1335, 1336
        , 1337, 1338, 1339, 1340, 1341, 1342, 1343, 1344, 1345, 1346, 1347
        , 1348, 1349, 1350, 1351, 1352, 1353, 1354, 1355, 1356, 1357, 1358
        , 1359, 1360, 1361, 1362, 1363, 1364, 1365, 1368, 1369, 1370, 1371
        , 1372, 1373, 1374, 1375, 1376, 1377, 1378, 1379, 1380, 1381, 1382
        , 1383, 1384, 1385, 1386, 1387, 1388, 1389, 1390, 1391, 1392, 1393
        , 1394, 1395, 1396, 1397, 1398, 1399, 1400, 1401, 1402, 1403, 1404
        , 1405, 1406, 1407, 1408, 1409, 1410, 1411, 1412, 1413, 1414, 1415
        , 1416, 1417, 1418, 1419, 1420, 1421, 1422, 1423, 1424, 1425, 1426
        , 1428, 1429, 1430, 1431, 1432, 1433, 1434, 1435, 1436, 1437, 1438
        , 1439, 1440, 1441, 1442, 1443, 1444, 1445, 1446, 1447, 1448, 1449
        , 1450, 1451, 1452, 1453, 1454, 1455, 1456, 1457, 1458, 1459, 1460
        , 1461, 1462, 1463, 1464, 1465, 1466, 1467, 1468, 1469, 1470, 1471
        , 1472, 1473, 1474, 1475, 1476, 1477, 1478, 1479, 1480, 1481, 1482
        , 1483, 1484, 1485, 1486, 1487, 1488, 1489, 1490, 1491, 1492, 1493
        , 1494, 1495, 1496, 1497, 1498, 1499, 1500, 1501, 1502, 1503, 1504
        , 1505, 1506, 1507, 1508, 1509, 1510, 1511, 1512, 1513, 1514, 1515
        , 1516, 1517, 1518, 1519, 1520, 1521, 1522, 1523, 1524, 1525, 1526
        , 1527, 1528, 1529, 1530, 1531, 1532, 1533, 1534, 1535, 1536, 1537
        , 1538, 1539, 1540, 1541, 1542, 1543, 1544, 1545, 1546, 1547, 1548
        , 1549, 1550, 1551, 1552, 1553, 1554, 1555, 1556, 1557, 1558, 1559
        , 1560, 1561, 1562, 1563, 1564, 1565, 1566, 1567, 1568, 1569, 1570
        , 1571, 1572, 1573, 1574, 1575, 1576, 1577, 1578, 1579, 1580, 1581
        , 1582, 1583, 1584, 1585, 1586, 1587, 1588, 1589, 1590, 1591, 1592
        , 1593, 1594, 1595, 1596, 1597, 1598, 1599, 1600, 1601, 1602, 1603
        , 1604, 1605, 1606, 1607, 1608, 1609, 1610, 1611, 1612, 1613, 1614
        , 1615, 1616, 1617, 1618, 1619, 1620, 1621, 1622, 1623, 1624, 1625
        , 1626, 1627, 1628, 1629, 1630, 1631, 1632, 1633, 1634, 1635, 1636
        , 1637, 1638, 1639, 1640, 1641, 1642, 1643, 1644, 1645, 1646, 1647
        , 1648, 1649, 1650, 1651, 1652, 1653, 1654, 1655, 1656, 1657, 1658
        , 1659, 1660, 1661, 1662, 1663, 1664, 1665, 1666, 1667, 1668, 1669
        , 1670, 1671, 1672, 1673, 1674, 1675, 1676, 1677, 1678, 1679, 1680
        , 1681, 1682, 1683, 1684, 1685, 1686, 1687, 1688, 1689, 1690, 1691
        , 1692, 1693, 1694, 1695, 1696, 1697, 1698, 1699, 1700, 1701, 1702
        , 1703, 1704, 1705, 1706, 1707, 1708, 1709, 1710, 1711, 1712, 1713
        , 1714, 1715, 1716, 1717, 1730, 1735, 3001, 3002, 3003, 3005, 3006
        , 3007, 3008, 3009, 3010, 3011, 3012, 3013, 3014, 3015, 3016, 3017
        , 3018, 3019, 3020, 3021, 3022, 3023, 3024, 3025, 3026, 3027, 3028
        , 3029, 3030, 3031, 3032, 3033, 3034, 3035, 3036, 3037, 3038, 3039
        , 3040, 3041, 3042, 3043, 3044, 3046, 3047, 3048, 3049, 3050, 3051
        , 3052, 3053, 3055, 3057, 3060, 3061, 3062, 3063, 3064, 3065, 3066
        , 3067, 3068, 3069, 3070, 3071, 3072, 3073, 3074, 3075, 3076, 3077
        , 3078, 3079, 3080, 3081, 3082, 3083, 3084, 3085, 3086, 3087, 3088
        , 3089, 3090, 3091, 3092, 3093, 3094, 3095, 3096, 3097, 3098, 3099
        , 3100, 3101, 3102, 3103, 3104, 3105, 3106, 3107, 3108, 3109, 3110
        , 3111, 3112, 3114, 3115, 3116, 3117, 3118, 3119, 3120, 3121, 3122
        , 3123, 3124, 3125, 3126, 3127, 3128, 3129, 3130, 3131, 3132, 3133
        , 3134, 3135, 3136, 3137, 3138, 3139, 3140, 3141, 3142, 3143, 3144
        , 3145, 3146, 3147, 3148, 3149, 3151, 3152, 3153, 3154, 3155, 3156
        , 3157, 3158, 3159, 3160, 3162, 3163, 3164, 3165, 3166, 3167, 3168
        , 3169, 3170, 3171, 3172, 3173, 3174, 3175, 3176, 3177, 3178, 3179
        , 3180, 3182, 3183, 3184, 3185, 3186, 3187, 3188, 3189, 3190, 3191
        , 3192, 3193, 3194, 3195, 3197, 3198, 3199, 3200, 3201, 3202, 3203
        , 3204, 3205, 3206, 3207, 3208, 3209, 3210, 3211, 3212, 3213, 3214
        , 3215, 3216, 3217, 3218, 3219, 3220, 3221, 3222, 3223, 3224, 3226
        , 3227, 3229, 3230, 3231, 3232, 3233, 3234, 3235, 3236, 3237, 3238
        , 3240, 3241, 3242, 3244, 3245, 3246, 3247, 3248, 3249, 3250, 3251
        , 3252, 3253, 3254, 3255, 3256, 3257, 3258, 3259, 3260, 3262, 3263
        , 3264, 3265, 3266, 3267, 3268, 3269, 3270, 3271, 3272, 3273, 3274
        , 3275, 3276, 3277, 3278, 3279, 3280, 3281, 3283, 3284, 3285, 3286
        , 3287, 3288, 3289, 3290, 3291, 3292, 3293, 3294, 3295, 3296, 3297
        , 3298, 3299, 3300, 3301, 3302, 3303, 3305, 3306, 3308, 3309, 3311
        , 3312, 3313, 3314, 3315, 3316, 3317, 3319, 3320, 3321, 3322, 3323
        , 3324, 3325, 3326, 3327, 3328, 3329, 3330, 3331, 3332, 3333, 3334
        , 3335, 3336, 3337, 3338, 3339, 3340, 3341, 3342, 3343, 3344, 3345
        , 3346, 3347, 3348, 3349, 3351, 3353, 3354, 3355, 3356, 3357, 3358
        , 3359, 3360, 3361, 3362, 3363, 3364, 3365, 3366, 3368, 3369, 3370
        , 3371, 3372, 3373, 3374, 3375, 3376, 3377, 3378, 3379, 3381, 3382
        , 3383, 3384, 3385, 3386, 3387, 3389, 3390, 3391, 3392, 3393, 3394
        , 3395, 3396, 3397, 3398, 3399, 3400, 3402, 3403, 3404, 3405, 3406
        , 3407, 3408, 3409, 3410, 3411, 3412, 3413, 3414, 3415, 3416, 3417
        , 3418, 3419, 3420, 3421, 3422, 3423, 3424, 3425, 3426, 3428, 3429
        , 3430, 3431, 3432, 3433, 3434, 3435, 3436, 3437, 3438, 3439, 3440
        , 3441, 3442, 3444, 3445, 3446, 3447, 3448, 3449, 3450, 3451, 3452
        , 3453, 3454, 3455, 3456, 3457, 3458, 3459, 3460, 3461, 3462, 3463
        , 3464, 3465, 3466, 3467, 3468, 3469, 3470, 3471, 3472, 3473, 3474
        , 3475, 3476, 3477, 3478, 3479, 3480, 3481, 3482, 3483, 3484, 3485
        , 3486, 3487, 3488, 3489, 3490, 3491, 3492, 3493, 3494, 3495, 3496
        , 3497, 3498, 3499, 3500, 3501, 3502, 3503, 3504, 3505, 3506, 3508
        , 3509, 3510, 3511, 3512, 3513, 3514, 3515, 3516, 3517, 3518, 3519
        , 3520, 3521, 3522, 3524, 3525, 3526, 3527, 3528, 3529, 3530, 3532
        , 3533, 3535, 3536, 3537, 3538, 3539, 3540, 3541, 3542, 3543, 3544
        , 3545, 3546, 3547, 3549, 3550, 3551, 3552, 3553, 3554, 3555, 3556
        , 3557, 3558, 3559, 3560, 3561, 3562, 3563, 3564, 3565, 3566, 3567
        , 3568, 3569, 3570, 3571, 3573, 3574, 3575, 3576, 3577, 3578, 3579
        , 3580, 3581, 3582, 3583, 3584, 3585, 3586, 3587, 3588, 3589, 3590
        , 3591, 3592, 3593, 3594, 3595, 3596, 3597, 3598, 3599, 3600, 3601
        , 3602, 3603, 3604, 3605, 3606, 3607, 3608, 3609, 3610, 3611, 3612
        , 3613, 3614, 3615, 3616, 3617, 3618, 3619, 3620, 3621, 3622, 3623
        , 3624, 3625, 3626, 3627, 3628, 3629, 3630, 3631, 3632, 3633, 3634
        , 3635, 3636, 3637, 3638, 3639, 3640, 3642, 3643, 3644, 3645, 3646
        , 3647, 3648, 3649, 3650, 3651, 3652, 3653, 3654, 3655, 3656, 3657
        , 3658, 3659, 3660, 3661, 3662, 3663, 3664, 3665, 3666, 3667, 3668
        , 3669, 3670, 3671, 3672, 3673, 3674, 3675, 3676, 3677, 3678, 3679
        , 3680, 3681, 3682, 3683, 3684, 3685, 3686, 3687, 3688, 3689, 3690
        , 3691, 3692, 3693, 3694, 3695, 3696, 3697, 3698, 3699, 3700, 3701
        , 3702, 3703, 3704, 3705, 3706, 3707, 3708, 3709, 3710, 3711, 3712
        , 3713, 3714, 3715, 3716, 3717, 3718, 3719, 3720, 3721, 3722, 3723
        , 3724, 3725, 3726, 3727, 3728, 3729, 3730, 3731, 3732, 3733, 3734
        , 3735, 3736, 3737, 3738, 3739, 3740, 3741, 3742, 3743, 3744, 3746
        , 3747, 3748, 3749, 3750, 3751, 3752, 3753, 3754, 3755, 3756, 3757
        , 3758, 3759, 3760, 3761, 3762, 3763, 3764, 3765, 3766, 3767, 3769
        , 3770, 3771, 3772, 3773, 3774, 3775, 3776, 3777, 3778, 3779, 3780
        , 3781, 3783, 3784, 3785, 3786, 3787, 3788, 3789, 3790, 3791, 3792
        , 3793, 3794, 3795, 3796, 3797, 3799, 3800, 3801, 3802, 3803, 3804
        , 3805, 3806, 3807, 3808, 3809, 3810, 3811, 3812, 3813, 3814, 3815
        , 3816, 3817, 3818, 3819, 3820, 3821, 3822, 3823, 3824, 3825, 3826
        , 3827, 3828, 3829, 3830, 3831, 3832, 3833, 3834, 3835, 3836, 3837
        , 3838, 3839, 3840, 3841, 3842, 3843, 3844, 3845, 3846, 3847, 3848
        , 3849, 3850, 3851, 3852, 3853, 3854, 3855, 3856, 3857, 3858, 3859
        , 3860, 3861, 3862, 3863, 3864, 3865, 3866, 3867, 3868, 3869, 3870
        , 3871, 3872, 3873, 3874, 3875, 3876, 3877, 3878, 3879, 3880, 3881
        , 3882, 3883, 3884, 3885, 3886, 3887, 3888, 3889, 3890, 3891, 3892
        , 3893, 3894, 3895, 3896, 3897, 3898, 3899, 3900, 3901, 3902, 3903
        , 3904, 3905, 3906, 3907, 3908, 3909, 3910, 3911, 3912, 3913, 3914
        , 3915, 3916, 3917, 3918, 3919, 3920, 3921, 3922, 3923, 3924, 3925
        , 3926, 3927, 3928, 3929, 3930, 3931, 3932, 3933, 3934, 3935, 3936
        , 3937, 3938, 3939, 3940, 3941, 3942, 3943, 3944, 3945, 3946, 3947
        , 3948, 3949, 3950, 3951, 3952, 3953, 3954, 3955, 3956, 3957, 3958
        , 3959, 3960, 3961, 3962, 3963, 3964, 3965, 3966, 3967, 3968, 3969
        , 3970, 3971, 3972, 3973, 3974, 3975, 3976, 3977, 3978, 3979, 3980
        , 3981, 3982, 3983, 3984, 3985, 3986, 3987, 3988, 3989, 3990, 3991
        , 3992, 3993, 3994, 3995, 3996, 3997, 3998, 3999, 4001, 4002, 4003
        , 4004, 4005, 4006, 4007, 4008, 4009, 4011, 4012, 4013, 4014, 4015
        , 4016, 4017, 4018, 4019, 4020, 4021, 4022, 4023, 4024, 4025, 4026
        , 4027, 4028, 4029, 4030, 4031, 4032, 4033, 4035, 4036, 4037, 4038
        , 4039, 4040, 4041, 4042, 4043, 4044, 4045, 4046, 4047, 4048, 4049
        , 4050, 4052, 4054, 4055, 4056, 4057, 4058, 4059, 4060, 4061, 4063
        , 4064, 4066, 4067, 4068, 4069, 4070, 4071, 4072, 4073, 4074, 4075
        , 4076, 4078, 4079, 4080, 4081, 4082, 4083, 4084, 4085, 4086, 4087
        , 4088, 4089, 4090, 4091, 4092, 4093, 4094, 4095, 4096, 4098, 4099
        , 4100, 4101, 4102, 4103, 4104, 4105, 4106, 4107, 4108, 4109, 4110
        , 4111, 4112, 4113, 4114, 4115, 4116, 4117, 5001, 5002, 5003, 5004
        , 5005, 5006, 5007, 5008, 5009, 5010, 5011, 5012, 5013, 5014, 5015
        , 5016, 5017, 5018, 5019, 5020, 5021, 5022, 5023, 5024, 5025, 5026
        , 5027, 5028, 5029, 5030, 5031, 5032, 5033, 5034, 5035, 5036, 5037
        , 5038, 5039, 5040, 5041, 5042, 5043, 5044, 5045, 5046, 5047, 5048
        , 5049, 5050, 5051, 5052, 5053, 5054, 5055, 5056, 5057, 5058, 5059
        , 5060, 5061, 5062, 5063, 5064, 5065, 5066, 5067, 5068, 5069, 5070
        , 5071, 5072, 5073, 5074, 5075, 5076, 5077, 5078, 5079, 5080, 5081
        , 5082, 5083, 5084, 5085, 5086, 5087, 5088, 5089, 5090, 5091, 5092
        , 5093, 5094, 5095, 5096, 5098, 5099, 5100, 5101, 5102, 5103, 5104
        , 5105, 5106, 5107, 5108, 5109, 5110, 5111, 5112, 5113, 5114, 5115
        , 5116, 5117, 5118, 5119, 5120, 5121, 5122, 5123, 5124, 5125, 5126
        , 5127, 5128, 5129, 5130, 5131, 5132, 5133, 5134, 5135, 5136, 5137
        , 5138, 5139, 5140, 5141, 5142, 5143, 5148, 5149, 5150, 5151, 5152
        , 5153, 5154, 5155, 5156, 5157, 5158, 5159, 5160, 5161, 5162, 5163
        , 5164, 5165, 5166, 5167, 5168, 5170, 5171, 5172, 5173, 5174, 5175
        , 5176, 5177, 5178, 5179, 5180, 5181, 5182, 5183, 5184, 5185, 5186
        , 5187, 5188, 5189, 5190, 5191, 5192, 5193, 5194, 5195, 5196, 5197
        , 5198, 5199, 5200, 5201, 5202, 5203, 5204, 5205, 5206, 5207, 5208
        , 5209, 5210, 5211, 5212, 5213, 5214, 5215, 5216, 5217, 5218, 5219
        , 5220, 5221, 5222, 5223, 5224, 5225, 5226, 5227, 5228, 5229, 5230
        , 5231, 5233, 5234, 5235, 5236, 5237, 5238, 5239, 5240, 5241, 5242
        , 5243, 5244, 5245, 5246, 5247, 5248, 5249, 5250, 5251, 5252, 5253
        , 5254, 5255, 5256, 5257, 5258, 5259, 5260, 5261, 5262, 5263, 5264
        , 5265, 5266, 5267, 5268, 5269, 5270, 5271, 5272, 5273, 5274, 5275
        , 5276, 5277, 5278, 5280, 5281, 5282, 5283, 5284, 5286, 5287, 5288
        , 5289, 5290, 5291, 5292, 5293, 5294, 5295, 5296, 5297, 5298, 5299
        , 5300, 5302, 5303, 5304, 5305, 5307, 5308, 5309, 5310, 5311, 5312
        , 5313, 5314, 5315, 5316, 5317, 5318, 5319, 5320, 5321, 5322, 5323
        , 5324, 5325, 5326, 5327, 5328, 5330, 5331, 5332, 5333, 5334, 5335
        , 5338, 5339, 5340, 5341, 5342, 5343, 5344, 5345, 5346, 5348, 5350
        , 5351, 5352, 5353, 5354, 5355, 5356, 5357, 5358, 5359, 5360, 5361
        , 5362, 5363, 5364, 5365, 5366, 5367, 5368, 5369, 5370, 5371, 5372
        , 5373, 5374, 5375, 5376, 5377, 5378, 5379, 5380, 5381, 5382, 5383
        , 5384, 5385, 5386, 5387, 5388, 5389, 5390, 5391, 5392, 5393, 5394
        , 5395, 5396, 5397, 5398, 5399, 5400, 5401, 5402, 5403, 5404, 5405
        , 5406, 5407, 5408, 5409, 5410, 5411, 5412, 5413, 5414, 5415, 5416
        , 5417, 5418, 5419, 5420, 5421, 5422, 5423, 5424, 5425, 5426, 5427
        , 5428, 5429, 5430, 5431, 5432, 5433, 5434, 5435, 5436, 5437, 5438
        , 5439, 5440, 5441, 5443, 5444, 5445, 5446, 5447, 5448, 5449, 5450
        , 5451, 5452, 5453, 5454, 5455, 5456, 5457, 5458, 5459, 5460, 5461
        , 5462, 5463, 5464, 5465, 5466, 5467, 5468, 5469, 5471, 5472, 5473
        , 5474, 5475, 5477, 5478, 5479, 5480, 5481, 5482, 5483, 5484, 5485
        , 5486, 5487, 5488, 5489, 5490, 5491, 5492, 5493, 5494, 5495, 5496
        , 5497, 5498, 5499, 5500, 5501, 5502, 5503, 5504, 5505, 5506, 5507
        , 5508, 5509, 5510, 5512, 5513, 5514, 5515, 5516, 5517, 5518, 5519
        , 5520, 5521, 5522, 5523, 5524, 5525, 5526, 5527, 5528, 5529, 5530
        , 5531, 5532, 5533, 5534, 5535, 5536, 5537, 5538, 5539, 5540, 5541
        , 5542, 5543, 5544, 5545, 5546, 5547, 5548, 5549, 5550, 5551, 5552
        , 5553, 5554, 5555, 5556, 5557, 5558, 5559, 5560, 5561, 5562, 5563
        , 5564, 5565, 5566, 5567, 5568, 5569, 5570, 5571, 5572, 5573, 5574
        , 5575, 5576, 5577, 5578, 5579, 5580, 5581, 5582, 5583, 5584, 5585
        , 5586, 5587, 5588, 5589, 5590, 5591, 5592, 5593, 5594, 5595, 5596
        , 5597, 5598, 5599, 5600, 5601, 5602, 5603, 5604, 5605, 5606, 5607
        , 5608, 5609, 5610, 5611, 5612, 5613, 5614, 5615, 5616, 5617, 5618
        , 5619, 5620, 5621, 5622, 5623, 5624, 5625, 5626, 5627, 5628, 5629
        , 5630, 5631, 5632, 5633, 5634, 5635, 5636, 5637, 5638, 6001, 6002
        , 6003, 6004, 6005, 6007, 6008, 6009, 6010, 6011, 6012, 6013, 6014
        , 6015, 6016, 6017, 6018, 6019, 6020, 6021, 6022, 6023, 6024, 6025
        , 6026, 6027, 6028, 6029, 6030, 6031, 6032, 6033, 6034, 6035, 6036
        , 6037, 6038, 6040, 6041, 6042, 6043, 6044, 6045, 6046, 6047, 6048
        , 6049, 6050, 6051, 6052, 6053, 6054, 6055, 6056, 6057, 6058, 6059
        , 6060, 6061, 6062, 6063, 6064, 6065, 6066, 6067, 6068, 6069, 6070
        , 6071, 6072, 6073, 6074, 6075, 6076, 6077, 6078, 6079, 6080, 6081
        , 6082, 6083, 6084, 6085, 6086, 6087, 6088, 6089, 6090, 6091, 7001
        , 7002, 7003, 7004, 7005, 7006, 7007, 7008, 7009, 7010, 7011, 7012
        , 7013, 7014, 7015, 7016, 7017, 7018, 7019, 7020, 7021, 7022, 7023
        , 7024, 7025, 7026, 7027, 7028, 7029, 7030, 7031, 7032, 7033, 7034
        , 7035, 7036, 7037, 7038, 7039, 7041, 7042, 7043, 7044, 7602, 7603
        , 7604, 7605, 7606, 7607, 7608, 7609, 9001, 9002, 9003, 9004, 9005
        , 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015, 9016
        , 9017, 9018, 9019, 9020, 9021, 9022, 9023, 9024, 9025, 9026, 9027
        , 9028, 9029, 9030, 9031, 9033, 9034, 9035, 9037, 9038, 9039, 9040
        , 9041, 9042, 9043, 9044, 9045, 9046, 9047, 9048, 9049, 9050, 9051
        , 9052, 9053, 9054, 9999
        ]

  render (CustomPatent x) = show x
