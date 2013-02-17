var TypeScript;
(function (TypeScript) {
    TypeScript.LexEOF = (-1);
    TypeScript.LexCodeNWL = 10;
    TypeScript.LexCodeRET = 13;
    TypeScript.LexCodeLS = 8232;
    TypeScript.LexCodePS = 8233;
    TypeScript.LexCodeTAB = 9;
    TypeScript.LexCodeVTAB = 11;
    TypeScript.LexCode_e = 'e'.charCodeAt(0);
    TypeScript.LexCode_E = 'E'.charCodeAt(0);
    TypeScript.LexCode_x = 'x'.charCodeAt(0);
    TypeScript.LexCode_X = 'X'.charCodeAt(0);
    TypeScript.LexCode_a = 'a'.charCodeAt(0);
    TypeScript.LexCode_A = 'A'.charCodeAt(0);
    TypeScript.LexCode_f = 'f'.charCodeAt(0);
    TypeScript.LexCode_F = 'F'.charCodeAt(0);
    TypeScript.LexCode_g = 'g'.charCodeAt(0);
    TypeScript.LexCode_m = 'm'.charCodeAt(0);
    TypeScript.LexCode_i = 'i'.charCodeAt(0);
    TypeScript.LexCode_u = 'u'.charCodeAt(0);
    TypeScript.LexCode_0 = '0'.charCodeAt(0);
    TypeScript.LexCode_9 = '9'.charCodeAt(0);
    TypeScript.LexCode_8 = '8'.charCodeAt(0);
    TypeScript.LexCode_7 = '7'.charCodeAt(0);
    TypeScript.LexCodeBSL = '\\'.charCodeAt(0);
    TypeScript.LexCodeSHP = '#'.charCodeAt(0);
    TypeScript.LexCodeBNG = '!'.charCodeAt(0);
    TypeScript.LexCodeQUO = '"'.charCodeAt(0);
    TypeScript.LexCodeAPO = '\''.charCodeAt(0);
    TypeScript.LexCodePCT = '%'.charCodeAt(0);
    TypeScript.LexCodeAMP = '&'.charCodeAt(0);
    TypeScript.LexCodeLPR = '('.charCodeAt(0);
    TypeScript.LexCodeRPR = ')'.charCodeAt(0);
    TypeScript.LexCodePLS = '+'.charCodeAt(0);
    TypeScript.LexCodeMIN = '-'.charCodeAt(0);
    TypeScript.LexCodeMUL = '*'.charCodeAt(0);
    TypeScript.LexCodeSLH = '/'.charCodeAt(0);
    TypeScript.LexCodeXOR = '^'.charCodeAt(0);
    TypeScript.LexCodeCMA = ','.charCodeAt(0);
    TypeScript.LexCodeDOT = '.'.charCodeAt(0);
    TypeScript.LexCodeLT = '<'.charCodeAt(0);
    TypeScript.LexCodeEQ = '='.charCodeAt(0);
    TypeScript.LexCodeGT = '>'.charCodeAt(0);
    TypeScript.LexCodeQUE = '?'.charCodeAt(0);
    TypeScript.LexCodeLBR = '['.charCodeAt(0);
    TypeScript.LexCodeRBR = ']'.charCodeAt(0);
    TypeScript.LexCodeUSC = '_'.charCodeAt(0);
    TypeScript.LexCodeLC = '{'.charCodeAt(0);
    TypeScript.LexCodeRC = '}'.charCodeAt(0);
    TypeScript.LexCodeBAR = '|'.charCodeAt(0);
    TypeScript.LexCodeTIL = '~'.charCodeAt(0);
    TypeScript.LexCodeCOL = ':'.charCodeAt(0);
    TypeScript.LexCodeSMC = ';'.charCodeAt(0);
    TypeScript.LexCodeUnderscore = '_'.charCodeAt(0);
    TypeScript.LexCodeDollar = '$'.charCodeAt(0);
    TypeScript.LexCodeSpace = 32;
    TypeScript.LexCodeAtSign = '@'.charCodeAt(0);
    TypeScript.LexCodeASCIIChars = 128;
    TypeScript.LexKeywordTable = undefined;
    var autoToken = new Array(TypeScript.LexCodeASCIIChars);
    var lexIdStartTable = new Array(TypeScript.LexCodeASCIIChars);
    var unicodeES3IdStart = [
        170, 
        170, 
        181, 
        181, 
        186, 
        186, 
        192, 
        214, 
        216, 
        246, 
        248, 
        543, 
        546, 
        563, 
        592, 
        685, 
        688, 
        696, 
        699, 
        705, 
        720, 
        721, 
        736, 
        740, 
        750, 
        750, 
        890, 
        890, 
        902, 
        902, 
        904, 
        906, 
        908, 
        908, 
        910, 
        929, 
        931, 
        974, 
        976, 
        983, 
        986, 
        1011, 
        1024, 
        1153, 
        1164, 
        1220, 
        1223, 
        1224, 
        1227, 
        1228, 
        1232, 
        1269, 
        1272, 
        1273, 
        1329, 
        1366, 
        1369, 
        1369, 
        1377, 
        1415, 
        1488, 
        1514, 
        1520, 
        1522, 
        1569, 
        1594, 
        1600, 
        1610, 
        1649, 
        1747, 
        1749, 
        1749, 
        1765, 
        1766, 
        1786, 
        1788, 
        1808, 
        1808, 
        1810, 
        1836, 
        1920, 
        1957, 
        2309, 
        2361, 
        2365, 
        2365, 
        2384, 
        2384, 
        2392, 
        2401, 
        2437, 
        2444, 
        2447, 
        2448, 
        2451, 
        2472, 
        2474, 
        2480, 
        2482, 
        2482, 
        2486, 
        2489, 
        2524, 
        2525, 
        2527, 
        2529, 
        2544, 
        2545, 
        2565, 
        2570, 
        2575, 
        2576, 
        2579, 
        2600, 
        2602, 
        2608, 
        2610, 
        2611, 
        2613, 
        2614, 
        2616, 
        2617, 
        2649, 
        2652, 
        2654, 
        2654, 
        2674, 
        2676, 
        2693, 
        2699, 
        2701, 
        2701, 
        2703, 
        2705, 
        2707, 
        2728, 
        2730, 
        2736, 
        2738, 
        2739, 
        2741, 
        2745, 
        2749, 
        2749, 
        2768, 
        2768, 
        2784, 
        2784, 
        2821, 
        2828, 
        2831, 
        2832, 
        2835, 
        2856, 
        2858, 
        2864, 
        2866, 
        2867, 
        2870, 
        2873, 
        2877, 
        2877, 
        2908, 
        2909, 
        2911, 
        2913, 
        2949, 
        2954, 
        2958, 
        2960, 
        2962, 
        2965, 
        2969, 
        2970, 
        2972, 
        2972, 
        2974, 
        2975, 
        2979, 
        2980, 
        2984, 
        2986, 
        2990, 
        2997, 
        2999, 
        3001, 
        3077, 
        3084, 
        3086, 
        3088, 
        3090, 
        3112, 
        3114, 
        3123, 
        3125, 
        3129, 
        3168, 
        3169, 
        3205, 
        3212, 
        3214, 
        3216, 
        3218, 
        3240, 
        3242, 
        3251, 
        3253, 
        3257, 
        3294, 
        3294, 
        3296, 
        3297, 
        3333, 
        3340, 
        3342, 
        3344, 
        3346, 
        3368, 
        3370, 
        3385, 
        3424, 
        3425, 
        3461, 
        3478, 
        3482, 
        3505, 
        3507, 
        3515, 
        3517, 
        3517, 
        3520, 
        3526, 
        3585, 
        3632, 
        3634, 
        3635, 
        3648, 
        3654, 
        3713, 
        3714, 
        3716, 
        3716, 
        3719, 
        3720, 
        3722, 
        3722, 
        3725, 
        3725, 
        3732, 
        3735, 
        3737, 
        3743, 
        3745, 
        3747, 
        3749, 
        3749, 
        3751, 
        3751, 
        3754, 
        3755, 
        3757, 
        3760, 
        3762, 
        3763, 
        3773, 
        3773, 
        3776, 
        3780, 
        3782, 
        3782, 
        3804, 
        3805, 
        3840, 
        3840, 
        3904, 
        3911, 
        3913, 
        3946, 
        3976, 
        3979, 
        4096, 
        4129, 
        4131, 
        4135, 
        4137, 
        4138, 
        4176, 
        4181, 
        4256, 
        4293, 
        4304, 
        4342, 
        4352, 
        4441, 
        4447, 
        4514, 
        4520, 
        4601, 
        4608, 
        4614, 
        4616, 
        4678, 
        4680, 
        4680, 
        4682, 
        4685, 
        4688, 
        4694, 
        4696, 
        4696, 
        4698, 
        4701, 
        4704, 
        4742, 
        4744, 
        4744, 
        4746, 
        4749, 
        4752, 
        4782, 
        4784, 
        4784, 
        4786, 
        4789, 
        4792, 
        4798, 
        4800, 
        4800, 
        4802, 
        4805, 
        4808, 
        4814, 
        4816, 
        4822, 
        4824, 
        4846, 
        4848, 
        4878, 
        4880, 
        4880, 
        4882, 
        4885, 
        4888, 
        4894, 
        4896, 
        4934, 
        4936, 
        4954, 
        5024, 
        5108, 
        5121, 
        5740, 
        5743, 
        5750, 
        5761, 
        5786, 
        5792, 
        5866, 
        6016, 
        6067, 
        6176, 
        6263, 
        6272, 
        6312, 
        7680, 
        7835, 
        7840, 
        7929, 
        7936, 
        7957, 
        7960, 
        7965, 
        7968, 
        8005, 
        8008, 
        8013, 
        8016, 
        8023, 
        8025, 
        8025, 
        8027, 
        8027, 
        8029, 
        8029, 
        8031, 
        8061, 
        8064, 
        8116, 
        8118, 
        8124, 
        8126, 
        8126, 
        8130, 
        8132, 
        8134, 
        8140, 
        8144, 
        8147, 
        8150, 
        8155, 
        8160, 
        8172, 
        8178, 
        8180, 
        8182, 
        8188, 
        8319, 
        8319, 
        8450, 
        8450, 
        8455, 
        8455, 
        8458, 
        8467, 
        8469, 
        8469, 
        8473, 
        8477, 
        8484, 
        8484, 
        8486, 
        8486, 
        8488, 
        8488, 
        8490, 
        8493, 
        8495, 
        8497, 
        8499, 
        8505, 
        8544, 
        8579, 
        12293, 
        12295, 
        12321, 
        12329, 
        12337, 
        12341, 
        12344, 
        12346, 
        12353, 
        12436, 
        12445, 
        12446, 
        12449, 
        12538, 
        12540, 
        12542, 
        12549, 
        12588, 
        12593, 
        12686, 
        12704, 
        12727, 
        13312, 
        13312, 
        19893, 
        19893, 
        19968, 
        19968, 
        40869, 
        40869, 
        40960, 
        42124, 
        44032, 
        44032, 
        55203, 
        55203, 
        63744, 
        64045, 
        64256, 
        64262, 
        64275, 
        64279, 
        64285, 
        64285, 
        64287, 
        64296, 
        64298, 
        64310, 
        64312, 
        64316, 
        64318, 
        64318, 
        64320, 
        64321, 
        64323, 
        64324, 
        64326, 
        64433, 
        64467, 
        64829, 
        64848, 
        64911, 
        64914, 
        64967, 
        65008, 
        65019, 
        65136, 
        65138, 
        65140, 
        65140, 
        65142, 
        65276, 
        65313, 
        65338, 
        65345, 
        65370, 
        65382, 
        65470, 
        65474, 
        65479, 
        65482, 
        65487, 
        65490, 
        65495, 
        65498, 
        65500
    ];
    var unicodeES3IdCont = [
        768, 
        846, 
        864, 
        866, 
        1155, 
        1158, 
        1425, 
        1441, 
        1443, 
        1465, 
        1467, 
        1469, 
        1471, 
        1471, 
        1473, 
        1474, 
        1476, 
        1476, 
        1611, 
        1621, 
        1632, 
        1641, 
        1648, 
        1648, 
        1750, 
        1756, 
        1759, 
        1764, 
        1767, 
        1768, 
        1770, 
        1773, 
        1776, 
        1785, 
        1809, 
        1809, 
        1840, 
        1866, 
        1958, 
        1968, 
        2305, 
        2307, 
        2364, 
        2364, 
        2366, 
        2381, 
        2385, 
        2388, 
        2402, 
        2403, 
        2406, 
        2415, 
        2433, 
        2435, 
        2492, 
        2492, 
        2494, 
        2500, 
        2503, 
        2504, 
        2507, 
        2509, 
        2519, 
        2519, 
        2530, 
        2531, 
        2534, 
        2543, 
        2562, 
        2562, 
        2620, 
        2620, 
        2622, 
        2626, 
        2631, 
        2632, 
        2635, 
        2637, 
        2662, 
        2673, 
        2689, 
        2691, 
        2748, 
        2748, 
        2750, 
        2757, 
        2759, 
        2761, 
        2763, 
        2765, 
        2790, 
        2799, 
        2817, 
        2819, 
        2876, 
        2876, 
        2878, 
        2883, 
        2887, 
        2888, 
        2891, 
        2893, 
        2902, 
        2903, 
        2918, 
        2927, 
        2946, 
        2947, 
        3006, 
        3010, 
        3014, 
        3016, 
        3018, 
        3021, 
        3031, 
        3031, 
        3047, 
        3055, 
        3073, 
        3075, 
        3134, 
        3140, 
        3142, 
        3144, 
        3146, 
        3149, 
        3157, 
        3158, 
        3174, 
        3183, 
        3202, 
        3203, 
        3262, 
        3268, 
        3270, 
        3272, 
        3274, 
        3277, 
        3285, 
        3286, 
        3302, 
        3311, 
        3330, 
        3331, 
        3390, 
        3395, 
        3398, 
        3400, 
        3402, 
        3405, 
        3415, 
        3415, 
        3430, 
        3439, 
        3458, 
        3459, 
        3530, 
        3530, 
        3535, 
        3540, 
        3542, 
        3542, 
        3544, 
        3551, 
        3570, 
        3571, 
        3633, 
        3633, 
        3636, 
        3642, 
        3655, 
        3662, 
        3664, 
        3673, 
        3761, 
        3761, 
        3764, 
        3769, 
        3771, 
        3772, 
        3784, 
        3789, 
        3792, 
        3801, 
        3864, 
        3865, 
        3872, 
        3881, 
        3893, 
        3893, 
        3895, 
        3895, 
        3897, 
        3897, 
        3902, 
        3903, 
        3953, 
        3972, 
        3974, 
        3975, 
        3984, 
        3991, 
        3993, 
        4028, 
        4038, 
        4038, 
        4140, 
        4146, 
        4150, 
        4153, 
        4160, 
        4169, 
        4182, 
        4185, 
        4969, 
        4977, 
        6068, 
        6099, 
        6112, 
        6121, 
        6160, 
        6169, 
        6313, 
        6313, 
        8255, 
        8256, 
        8400, 
        8412, 
        8417, 
        8417, 
        12330, 
        12335, 
        12441, 
        12442, 
        12539, 
        12539, 
        64286, 
        64286, 
        65056, 
        65059, 
        65075, 
        65076, 
        65101, 
        65103, 
        65296, 
        65305, 
        65343, 
        65343, 
        65381, 
        65381
    ];
    var unicodeES5IdStart = [
        170, 
        170, 
        181, 
        181, 
        186, 
        186, 
        192, 
        214, 
        216, 
        246, 
        248, 
        705, 
        710, 
        721, 
        736, 
        740, 
        748, 
        748, 
        750, 
        750, 
        880, 
        884, 
        886, 
        887, 
        890, 
        893, 
        902, 
        902, 
        904, 
        906, 
        908, 
        908, 
        910, 
        929, 
        931, 
        1013, 
        1015, 
        1153, 
        1162, 
        1319, 
        1329, 
        1366, 
        1369, 
        1369, 
        1377, 
        1415, 
        1488, 
        1514, 
        1520, 
        1522, 
        1568, 
        1610, 
        1646, 
        1647, 
        1649, 
        1747, 
        1749, 
        1749, 
        1765, 
        1766, 
        1774, 
        1775, 
        1786, 
        1788, 
        1791, 
        1791, 
        1808, 
        1808, 
        1810, 
        1839, 
        1869, 
        1957, 
        1969, 
        1969, 
        1994, 
        2026, 
        2036, 
        2037, 
        2042, 
        2042, 
        2048, 
        2069, 
        2074, 
        2074, 
        2084, 
        2084, 
        2088, 
        2088, 
        2112, 
        2136, 
        2208, 
        2208, 
        2210, 
        2220, 
        2308, 
        2361, 
        2365, 
        2365, 
        2384, 
        2384, 
        2392, 
        2401, 
        2417, 
        2423, 
        2425, 
        2431, 
        2437, 
        2444, 
        2447, 
        2448, 
        2451, 
        2472, 
        2474, 
        2480, 
        2482, 
        2482, 
        2486, 
        2489, 
        2493, 
        2493, 
        2510, 
        2510, 
        2524, 
        2525, 
        2527, 
        2529, 
        2544, 
        2545, 
        2565, 
        2570, 
        2575, 
        2576, 
        2579, 
        2600, 
        2602, 
        2608, 
        2610, 
        2611, 
        2613, 
        2614, 
        2616, 
        2617, 
        2649, 
        2652, 
        2654, 
        2654, 
        2674, 
        2676, 
        2693, 
        2701, 
        2703, 
        2705, 
        2707, 
        2728, 
        2730, 
        2736, 
        2738, 
        2739, 
        2741, 
        2745, 
        2749, 
        2749, 
        2768, 
        2768, 
        2784, 
        2785, 
        2821, 
        2828, 
        2831, 
        2832, 
        2835, 
        2856, 
        2858, 
        2864, 
        2866, 
        2867, 
        2869, 
        2873, 
        2877, 
        2877, 
        2908, 
        2909, 
        2911, 
        2913, 
        2929, 
        2929, 
        2947, 
        2947, 
        2949, 
        2954, 
        2958, 
        2960, 
        2962, 
        2965, 
        2969, 
        2970, 
        2972, 
        2972, 
        2974, 
        2975, 
        2979, 
        2980, 
        2984, 
        2986, 
        2990, 
        3001, 
        3024, 
        3024, 
        3077, 
        3084, 
        3086, 
        3088, 
        3090, 
        3112, 
        3114, 
        3123, 
        3125, 
        3129, 
        3133, 
        3133, 
        3160, 
        3161, 
        3168, 
        3169, 
        3205, 
        3212, 
        3214, 
        3216, 
        3218, 
        3240, 
        3242, 
        3251, 
        3253, 
        3257, 
        3261, 
        3261, 
        3294, 
        3294, 
        3296, 
        3297, 
        3313, 
        3314, 
        3333, 
        3340, 
        3342, 
        3344, 
        3346, 
        3386, 
        3389, 
        3389, 
        3406, 
        3406, 
        3424, 
        3425, 
        3450, 
        3455, 
        3461, 
        3478, 
        3482, 
        3505, 
        3507, 
        3515, 
        3517, 
        3517, 
        3520, 
        3526, 
        3585, 
        3632, 
        3634, 
        3635, 
        3648, 
        3654, 
        3713, 
        3714, 
        3716, 
        3716, 
        3719, 
        3720, 
        3722, 
        3722, 
        3725, 
        3725, 
        3732, 
        3735, 
        3737, 
        3743, 
        3745, 
        3747, 
        3749, 
        3749, 
        3751, 
        3751, 
        3754, 
        3755, 
        3757, 
        3760, 
        3762, 
        3763, 
        3773, 
        3773, 
        3776, 
        3780, 
        3782, 
        3782, 
        3804, 
        3807, 
        3840, 
        3840, 
        3904, 
        3911, 
        3913, 
        3948, 
        3976, 
        3980, 
        4096, 
        4138, 
        4159, 
        4159, 
        4176, 
        4181, 
        4186, 
        4189, 
        4193, 
        4193, 
        4197, 
        4198, 
        4206, 
        4208, 
        4213, 
        4225, 
        4238, 
        4238, 
        4256, 
        4293, 
        4295, 
        4295, 
        4301, 
        4301, 
        4304, 
        4346, 
        4348, 
        4680, 
        4682, 
        4685, 
        4688, 
        4694, 
        4696, 
        4696, 
        4698, 
        4701, 
        4704, 
        4744, 
        4746, 
        4749, 
        4752, 
        4784, 
        4786, 
        4789, 
        4792, 
        4798, 
        4800, 
        4800, 
        4802, 
        4805, 
        4808, 
        4822, 
        4824, 
        4880, 
        4882, 
        4885, 
        4888, 
        4954, 
        4992, 
        5007, 
        5024, 
        5108, 
        5121, 
        5740, 
        5743, 
        5759, 
        5761, 
        5786, 
        5792, 
        5866, 
        5870, 
        5872, 
        5888, 
        5900, 
        5902, 
        5905, 
        5920, 
        5937, 
        5952, 
        5969, 
        5984, 
        5996, 
        5998, 
        6000, 
        6016, 
        6067, 
        6103, 
        6103, 
        6108, 
        6108, 
        6176, 
        6263, 
        6272, 
        6312, 
        6314, 
        6314, 
        6320, 
        6389, 
        6400, 
        6428, 
        6480, 
        6509, 
        6512, 
        6516, 
        6528, 
        6571, 
        6593, 
        6599, 
        6656, 
        6678, 
        6688, 
        6740, 
        6823, 
        6823, 
        6917, 
        6963, 
        6981, 
        6987, 
        7043, 
        7072, 
        7086, 
        7087, 
        7098, 
        7141, 
        7168, 
        7203, 
        7245, 
        7247, 
        7258, 
        7293, 
        7401, 
        7404, 
        7406, 
        7409, 
        7413, 
        7414, 
        7424, 
        7615, 
        7680, 
        7957, 
        7960, 
        7965, 
        7968, 
        8005, 
        8008, 
        8013, 
        8016, 
        8023, 
        8025, 
        8025, 
        8027, 
        8027, 
        8029, 
        8029, 
        8031, 
        8061, 
        8064, 
        8116, 
        8118, 
        8124, 
        8126, 
        8126, 
        8130, 
        8132, 
        8134, 
        8140, 
        8144, 
        8147, 
        8150, 
        8155, 
        8160, 
        8172, 
        8178, 
        8180, 
        8182, 
        8188, 
        8305, 
        8305, 
        8319, 
        8319, 
        8336, 
        8348, 
        8450, 
        8450, 
        8455, 
        8455, 
        8458, 
        8467, 
        8469, 
        8469, 
        8473, 
        8477, 
        8484, 
        8484, 
        8486, 
        8486, 
        8488, 
        8488, 
        8490, 
        8493, 
        8495, 
        8505, 
        8508, 
        8511, 
        8517, 
        8521, 
        8526, 
        8526, 
        8544, 
        8584, 
        11264, 
        11310, 
        11312, 
        11358, 
        11360, 
        11492, 
        11499, 
        11502, 
        11506, 
        11507, 
        11520, 
        11557, 
        11559, 
        11559, 
        11565, 
        11565, 
        11568, 
        11623, 
        11631, 
        11631, 
        11648, 
        11670, 
        11680, 
        11686, 
        11688, 
        11694, 
        11696, 
        11702, 
        11704, 
        11710, 
        11712, 
        11718, 
        11720, 
        11726, 
        11728, 
        11734, 
        11736, 
        11742, 
        11823, 
        11823, 
        12293, 
        12295, 
        12321, 
        12329, 
        12337, 
        12341, 
        12344, 
        12348, 
        12353, 
        12438, 
        12445, 
        12447, 
        12449, 
        12538, 
        12540, 
        12543, 
        12549, 
        12589, 
        12593, 
        12686, 
        12704, 
        12730, 
        12784, 
        12799, 
        13312, 
        13312, 
        19893, 
        19893, 
        19968, 
        19968, 
        40908, 
        40908, 
        40960, 
        42124, 
        42192, 
        42237, 
        42240, 
        42508, 
        42512, 
        42527, 
        42538, 
        42539, 
        42560, 
        42606, 
        42623, 
        42647, 
        42656, 
        42735, 
        42775, 
        42783, 
        42786, 
        42888, 
        42891, 
        42894, 
        42896, 
        42899, 
        42912, 
        42922, 
        43000, 
        43009, 
        43011, 
        43013, 
        43015, 
        43018, 
        43020, 
        43042, 
        43072, 
        43123, 
        43138, 
        43187, 
        43250, 
        43255, 
        43259, 
        43259, 
        43274, 
        43301, 
        43312, 
        43334, 
        43360, 
        43388, 
        43396, 
        43442, 
        43471, 
        43471, 
        43520, 
        43560, 
        43584, 
        43586, 
        43588, 
        43595, 
        43616, 
        43638, 
        43642, 
        43642, 
        43648, 
        43695, 
        43697, 
        43697, 
        43701, 
        43702, 
        43705, 
        43709, 
        43712, 
        43712, 
        43714, 
        43714, 
        43739, 
        43741, 
        43744, 
        43754, 
        43762, 
        43764, 
        43777, 
        43782, 
        43785, 
        43790, 
        43793, 
        43798, 
        43808, 
        43814, 
        43816, 
        43822, 
        43968, 
        44002, 
        44032, 
        44032, 
        55203, 
        55203, 
        55216, 
        55238, 
        55243, 
        55291, 
        63744, 
        64109, 
        64112, 
        64217, 
        64256, 
        64262, 
        64275, 
        64279, 
        64285, 
        64285, 
        64287, 
        64296, 
        64298, 
        64310, 
        64312, 
        64316, 
        64318, 
        64318, 
        64320, 
        64321, 
        64323, 
        64324, 
        64326, 
        64433, 
        64467, 
        64829, 
        64848, 
        64911, 
        64914, 
        64967, 
        65008, 
        65019, 
        65136, 
        65140, 
        65142, 
        65276, 
        65313, 
        65338, 
        65345, 
        65370, 
        65382, 
        65470, 
        65474, 
        65479, 
        65482, 
        65487, 
        65490, 
        65495, 
        65498, 
        65500
    ];
    var unicodeES5IdCont = [
        768, 
        879, 
        1155, 
        1159, 
        1425, 
        1469, 
        1471, 
        1471, 
        1473, 
        1474, 
        1476, 
        1477, 
        1479, 
        1479, 
        1552, 
        1562, 
        1611, 
        1641, 
        1648, 
        1648, 
        1750, 
        1756, 
        1759, 
        1764, 
        1767, 
        1768, 
        1770, 
        1773, 
        1776, 
        1785, 
        1809, 
        1809, 
        1840, 
        1866, 
        1958, 
        1968, 
        1984, 
        1993, 
        2027, 
        2035, 
        2070, 
        2073, 
        2075, 
        2083, 
        2085, 
        2087, 
        2089, 
        2093, 
        2137, 
        2139, 
        2276, 
        2302, 
        2304, 
        2307, 
        2362, 
        2364, 
        2366, 
        2383, 
        2385, 
        2391, 
        2402, 
        2403, 
        2406, 
        2415, 
        2433, 
        2435, 
        2492, 
        2492, 
        2494, 
        2500, 
        2503, 
        2504, 
        2507, 
        2509, 
        2519, 
        2519, 
        2530, 
        2531, 
        2534, 
        2543, 
        2561, 
        2563, 
        2620, 
        2620, 
        2622, 
        2626, 
        2631, 
        2632, 
        2635, 
        2637, 
        2641, 
        2641, 
        2662, 
        2673, 
        2677, 
        2677, 
        2689, 
        2691, 
        2748, 
        2748, 
        2750, 
        2757, 
        2759, 
        2761, 
        2763, 
        2765, 
        2786, 
        2787, 
        2790, 
        2799, 
        2817, 
        2819, 
        2876, 
        2876, 
        2878, 
        2884, 
        2887, 
        2888, 
        2891, 
        2893, 
        2902, 
        2903, 
        2914, 
        2915, 
        2918, 
        2927, 
        2946, 
        2946, 
        3006, 
        3010, 
        3014, 
        3016, 
        3018, 
        3021, 
        3031, 
        3031, 
        3046, 
        3055, 
        3073, 
        3075, 
        3134, 
        3140, 
        3142, 
        3144, 
        3146, 
        3149, 
        3157, 
        3158, 
        3170, 
        3171, 
        3174, 
        3183, 
        3202, 
        3203, 
        3260, 
        3260, 
        3262, 
        3268, 
        3270, 
        3272, 
        3274, 
        3277, 
        3285, 
        3286, 
        3298, 
        3299, 
        3302, 
        3311, 
        3330, 
        3331, 
        3390, 
        3396, 
        3398, 
        3400, 
        3402, 
        3405, 
        3415, 
        3415, 
        3426, 
        3427, 
        3430, 
        3439, 
        3458, 
        3459, 
        3530, 
        3530, 
        3535, 
        3540, 
        3542, 
        3542, 
        3544, 
        3551, 
        3570, 
        3571, 
        3633, 
        3633, 
        3636, 
        3642, 
        3655, 
        3662, 
        3664, 
        3673, 
        3761, 
        3761, 
        3764, 
        3769, 
        3771, 
        3772, 
        3784, 
        3789, 
        3792, 
        3801, 
        3864, 
        3865, 
        3872, 
        3881, 
        3893, 
        3893, 
        3895, 
        3895, 
        3897, 
        3897, 
        3902, 
        3903, 
        3953, 
        3972, 
        3974, 
        3975, 
        3981, 
        3991, 
        3993, 
        4028, 
        4038, 
        4038, 
        4139, 
        4158, 
        4160, 
        4169, 
        4182, 
        4185, 
        4190, 
        4192, 
        4194, 
        4196, 
        4199, 
        4205, 
        4209, 
        4212, 
        4226, 
        4237, 
        4239, 
        4253, 
        4957, 
        4959, 
        5906, 
        5908, 
        5938, 
        5940, 
        5970, 
        5971, 
        6002, 
        6003, 
        6068, 
        6099, 
        6109, 
        6109, 
        6112, 
        6121, 
        6155, 
        6157, 
        6160, 
        6169, 
        6313, 
        6313, 
        6432, 
        6443, 
        6448, 
        6459, 
        6470, 
        6479, 
        6576, 
        6592, 
        6600, 
        6601, 
        6608, 
        6617, 
        6679, 
        6683, 
        6741, 
        6750, 
        6752, 
        6780, 
        6783, 
        6793, 
        6800, 
        6809, 
        6912, 
        6916, 
        6964, 
        6980, 
        6992, 
        7001, 
        7019, 
        7027, 
        7040, 
        7042, 
        7073, 
        7085, 
        7088, 
        7097, 
        7142, 
        7155, 
        7204, 
        7223, 
        7232, 
        7241, 
        7248, 
        7257, 
        7376, 
        7378, 
        7380, 
        7400, 
        7405, 
        7405, 
        7410, 
        7412, 
        7616, 
        7654, 
        7676, 
        7679, 
        8204, 
        8205, 
        8255, 
        8256, 
        8276, 
        8276, 
        8400, 
        8412, 
        8417, 
        8417, 
        8421, 
        8432, 
        11503, 
        11505, 
        11647, 
        11647, 
        11744, 
        11775, 
        12330, 
        12335, 
        12441, 
        12442, 
        42528, 
        42537, 
        42607, 
        42607, 
        42612, 
        42621, 
        42655, 
        42655, 
        42736, 
        42737, 
        43010, 
        43010, 
        43014, 
        43014, 
        43019, 
        43019, 
        43043, 
        43047, 
        43136, 
        43137, 
        43188, 
        43204, 
        43216, 
        43225, 
        43232, 
        43249, 
        43264, 
        43273, 
        43302, 
        43309, 
        43335, 
        43347, 
        43392, 
        43395, 
        43443, 
        43456, 
        43472, 
        43481, 
        43561, 
        43574, 
        43587, 
        43587, 
        43596, 
        43597, 
        43600, 
        43609, 
        43643, 
        43643, 
        43696, 
        43696, 
        43698, 
        43700, 
        43703, 
        43704, 
        43710, 
        43711, 
        43713, 
        43713, 
        43755, 
        43759, 
        43765, 
        43766, 
        44003, 
        44010, 
        44012, 
        44013, 
        44016, 
        44025, 
        64286, 
        64286, 
        65024, 
        65039, 
        65056, 
        65062, 
        65075, 
        65076, 
        65101, 
        65103, 
        65296, 
        65305, 
        65343, 
        65343
    ];
    function LexLookUpUnicodeMap(code, map) {
        var lo = 0;
        var hi = map.length;
        var mid;
        while(lo + 1 < hi) {
            mid = lo + (hi - lo) / 2;
            mid -= mid % 2;
            if(map[mid] <= code && code <= map[mid + 1]) {
                return true;
            }
            if(code < map[mid]) {
                hi = mid;
            } else {
                lo = mid + 2;
            }
        }
        return false;
    }
    TypeScript.LexLookUpUnicodeMap = LexLookUpUnicodeMap;
    function LexIsUnicodeDigit(code) {
        if(TypeScript.codeGenTarget == TypeScript.CodeGenTarget.ES3) {
            return LexLookUpUnicodeMap(code, unicodeES3IdCont);
        } else {
            return LexLookUpUnicodeMap(code, unicodeES5IdCont);
        }
    }
    TypeScript.LexIsUnicodeDigit = LexIsUnicodeDigit;
    function LexIsUnicodeIdStart(code) {
        if(TypeScript.codeGenTarget == TypeScript.CodeGenTarget.ES3) {
            return LexLookUpUnicodeMap(code, unicodeES3IdStart);
        } else {
            return LexLookUpUnicodeMap(code, unicodeES5IdStart);
        }
    }
    TypeScript.LexIsUnicodeIdStart = LexIsUnicodeIdStart;
    function LexInitialize() {
        TypeScript.initializeStaticTokens();
        autoToken[TypeScript.LexCodeLPR] = TypeScript.staticTokens[TypeScript.TokenID.OpenParen];
        autoToken[TypeScript.LexCodeRPR] = TypeScript.staticTokens[TypeScript.TokenID.CloseParen];
        autoToken[TypeScript.LexCodeCMA] = TypeScript.staticTokens[TypeScript.TokenID.Comma];
        autoToken[TypeScript.LexCodeSMC] = TypeScript.staticTokens[TypeScript.TokenID.Semicolon];
        autoToken[TypeScript.LexCodeLBR] = TypeScript.staticTokens[TypeScript.TokenID.OpenBracket];
        autoToken[TypeScript.LexCodeRBR] = TypeScript.staticTokens[TypeScript.TokenID.CloseBracket];
        autoToken[TypeScript.LexCodeTIL] = TypeScript.staticTokens[TypeScript.TokenID.Tilde];
        autoToken[TypeScript.LexCodeQUE] = TypeScript.staticTokens[TypeScript.TokenID.Question];
        autoToken[TypeScript.LexCodeLC] = TypeScript.staticTokens[TypeScript.TokenID.OpenBrace];
        autoToken[TypeScript.LexCodeRC] = TypeScript.staticTokens[TypeScript.TokenID.CloseBrace];
        autoToken[TypeScript.LexCodeCOL] = TypeScript.staticTokens[TypeScript.TokenID.Colon];
        TypeScript.LexKeywordTable = new TypeScript.StringHashTable();
        for(var i in (TypeScript.TokenID)._map) {
            if((i) <= TypeScript.TokenID.LimKeyword) {
                TypeScript.LexKeywordTable.add((TypeScript.TokenID)._map[i].toLowerCase(), i);
            }
        }
        for(var j = 0; j < TypeScript.LexCodeASCIIChars; j++) {
            if(LexIsIdentifierStartChar(j)) {
                lexIdStartTable[j] = true;
            } else {
                lexIdStartTable[j] = false;
            }
        }
    }
    TypeScript.LexInitialize = LexInitialize;
    function LexAdjustIndent(code, indentAmt) {
        if((code == TypeScript.LexCodeLBR) || (code == TypeScript.LexCodeLC) || (code == TypeScript.LexCodeLPR)) {
            return indentAmt + 1;
        } else if((code == TypeScript.LexCodeRBR) || (code == TypeScript.LexCodeRC) || (code == TypeScript.LexCodeRPR)) {
            return indentAmt - 1;
        } else {
            return indentAmt;
        }
    }
    TypeScript.LexAdjustIndent = LexAdjustIndent;
    function LexIsIdentifierStartChar(code) {
        return (((code >= 97) && (code <= 122)) || ((code >= 65) && (code <= 90)) || (code == TypeScript.LexCodeDollar) || (code == TypeScript.LexCodeUnderscore));
    }
    TypeScript.LexIsIdentifierStartChar = LexIsIdentifierStartChar;
    function LexIsDigit(code) {
        return ((code >= 48) && (code <= 57));
    }
    TypeScript.LexIsDigit = LexIsDigit;
    function LexIsIdentifierChar(code) {
        return lexIdStartTable[code] || LexIsDigit(code);
    }
    TypeScript.LexIsIdentifierChar = LexIsIdentifierChar;
    function LexMatchingOpen(code) {
        if(code == TypeScript.LexCodeRBR) {
            return TypeScript.LexCodeLBR;
        } else if(code == TypeScript.LexCodeRC) {
            return TypeScript.LexCodeLC;
        } else if(code == TypeScript.LexCodeRPR) {
            return TypeScript.LexCodeLPR;
        } else {
            return 0;
        }
    }
    TypeScript.LexMatchingOpen = LexMatchingOpen;
    (function (NumberScanState) {
        NumberScanState._map = [];
        NumberScanState._map[0] = "Start";
        NumberScanState.Start = 0;
        NumberScanState._map[1] = "InFraction";
        NumberScanState.InFraction = 1;
        NumberScanState._map[2] = "InEmptyFraction";
        NumberScanState.InEmptyFraction = 2;
        NumberScanState._map[3] = "InExponent";
        NumberScanState.InExponent = 3;
    })(TypeScript.NumberScanState || (TypeScript.NumberScanState = {}));
    var NumberScanState = TypeScript.NumberScanState;
    (function (LexState) {
        LexState._map = [];
        LexState._map[0] = "Start";
        LexState.Start = 0;
        LexState._map[1] = "InMultilineComment";
        LexState.InMultilineComment = 1;
        LexState._map[2] = "InMultilineSingleQuoteString";
        LexState.InMultilineSingleQuoteString = 2;
        LexState._map[3] = "InMultilineDoubleQuoteString";
        LexState.InMultilineDoubleQuoteString = 3;
    })(TypeScript.LexState || (TypeScript.LexState = {}));
    var LexState = TypeScript.LexState;
    (function (LexMode) {
        LexMode._map = [];
        LexMode._map[0] = "Line";
        LexMode.Line = 0;
        LexMode._map[1] = "File";
        LexMode.File = 1;
    })(TypeScript.LexMode || (TypeScript.LexMode = {}));
    var LexMode = TypeScript.LexMode;
    (function (CommentStyle) {
        CommentStyle._map = [];
        CommentStyle._map[0] = "Line";
        CommentStyle.Line = 0;
        CommentStyle._map[1] = "Block";
        CommentStyle.Block = 1;
    })(TypeScript.CommentStyle || (TypeScript.CommentStyle = {}));
    var CommentStyle = TypeScript.CommentStyle;
    var StringSourceText = (function () {
        function StringSourceText(text) {
            this.text = text;
        }
        StringSourceText.prototype.getText = function (start, end) {
            return this.text.substring(start, end);
        };
        StringSourceText.prototype.getLength = function () {
            return this.text.length;
        };
        return StringSourceText;
    })();
    TypeScript.StringSourceText = StringSourceText;    
    var SourceTextSegment = (function () {
        function SourceTextSegment(segmentStart, segmentEnd, segment) {
            this.segmentStart = segmentStart;
            this.segmentEnd = segmentEnd;
            this.segment = segment;
        }
        SourceTextSegment.prototype.charCodeAt = function (index) {
            return this.segment.charCodeAt(index - this.segmentStart);
        };
        SourceTextSegment.prototype.substring = function (start, end) {
            return this.segment.substring(start - this.segmentStart, end - this.segmentStart);
        };
        return SourceTextSegment;
    })();
    TypeScript.SourceTextSegment = SourceTextSegment;    
    var AggerateSourceTextSegment = (function () {
        function AggerateSourceTextSegment(seg1, seg2) {
            this.seg1 = seg1;
            this.seg2 = seg2;
        }
        AggerateSourceTextSegment.prototype.charCodeAt = function (index) {
            if(this.seg1.segmentStart <= index && index < this.seg1.segmentEnd) {
                return this.seg1.segment.charCodeAt(index - this.seg1.segmentStart);
            }
            return this.seg2.segment.charCodeAt(index - this.seg2.segmentStart);
        };
        AggerateSourceTextSegment.prototype.substring = function (start, end) {
            if(this.seg1.segmentStart <= start && end <= this.seg1.segmentEnd) {
                return this.seg1.segment.substring(start - this.seg1.segmentStart, end - this.seg1.segmentStart);
            }
            return this.seg2.segment.substring(start - this.seg2.segmentStart) + this.seg1.segment.substring(0, end - this.seg1.segmentStart);
        };
        return AggerateSourceTextSegment;
    })();
    TypeScript.AggerateSourceTextSegment = AggerateSourceTextSegment;    
    var ScannerTextStream = (function () {
        function ScannerTextStream(sourceText) {
            this.sourceText = sourceText;
            this.agg = new AggerateSourceTextSegment(ScannerTextStream.emptySegment, ScannerTextStream.emptySegment);
            this.len = this.sourceText.getLength();
        }
        ScannerTextStream.emptySegment = new SourceTextSegment(0, 0, "");
        ScannerTextStream.prototype.max = function (a, b) {
            return a >= b ? a : b;
        };
        ScannerTextStream.prototype.min = function (a, b) {
            return a <= b ? a : b;
        };
        ScannerTextStream.prototype.fetchSegment = function (start, end) {
            if(this.agg.seg1.segmentStart <= start && end <= this.agg.seg1.segmentEnd) {
                return this.agg.seg1;
            }
            if(this.agg.seg2.segmentStart <= start && end <= this.agg.seg1.segmentEnd) {
                return this.agg;
            }
            var prev = this.agg.seg1;
            var s = prev.segmentEnd;
            var e = TypeScript.max(s + 512, end);
            e = TypeScript.min(e, this.len);
            var src = this.sourceText.getText(s, e);
            var newSeg = new SourceTextSegment(s, e, src);
            this.agg.seg2 = prev;
            this.agg.seg1 = newSeg;
            return this.agg;
        };
        ScannerTextStream.prototype.charCodeAt = function (index) {
            return this.fetchSegment(index, index + 1).charCodeAt(index);
        };
        ScannerTextStream.prototype.substring = function (start, end) {
            return this.fetchSegment(start, end).substring(start, end);
        };
        return ScannerTextStream;
    })();
    TypeScript.ScannerTextStream = ScannerTextStream;    
    var SavedTokens = (function () {
        function SavedTokens() {
            this.prevToken = null;
            this.curSavedToken = null;
            this.prevSavedToken = null;
            this.prevToken = null;
            this.currentToken = 0;
            this.tokens = new Array();
            this.seenUnicodeChar = false;
            this.seenUnicodeCharInComment = false;
            this.prevLine = 1;
            this.line = 1;
            this.col = 0;
            this.lexState = LexState.Start;
            this.commentStack = new Array();
            this.lineMap = [];
        }
        SavedTokens.prototype.previousToken = function () {
            return this.prevToken;
        };
        SavedTokens.prototype.close = function () {
            this.currentToken = 0;
        };
        SavedTokens.prototype.addToken = function (tok, scanner) {
            this.tokens[this.currentToken++] = new TypeScript.SavedToken(tok, scanner.startPos, scanner.pos);
        };
        SavedTokens.prototype.scan = function () {
            this.startLine = this.line;
            this.startPos = this.col;
            if(this.currentTokenIndex == this.currentTokens.length) {
                if(this.line < this.lineMap.length) {
                    this.line++;
                    this.col = 0;
                    this.currentTokenIndex = 0;
                    this.currentTokens = this.tokensByLine[this.line];
                } else {
                    return TypeScript.staticTokens[TypeScript.TokenID.EndOfFile];
                }
            }
            if(this.currentTokenIndex < this.currentTokens.length) {
                this.prevToken = this.curSavedToken.tok;
                this.prevSavedToken = this.curSavedToken;
                this.curSavedToken = this.currentTokens[this.currentTokenIndex++];
                var curToken = this.curSavedToken.tok;
                this.pos = this.curSavedToken.limChar;
                this.col += (this.curSavedToken.limChar - this.curSavedToken.minChar);
                this.startPos = this.curSavedToken.minChar;
                this.prevLine = this.line;
                return curToken;
            } else {
                return TypeScript.staticTokens[TypeScript.TokenID.EndOfFile];
            }
        };
        SavedTokens.prototype.syncToTok = function (offset) {
            this.line = getLineNumberFromPosition(this.lineMap, offset);
            this.currentTokenIndex = 0;
            var tmpCol = offset - this.lineMap[this.line];
            while((this.lexStateByLine[this.line] == LexState.InMultilineComment) && (this.line > 0)) {
                this.line--;
                tmpCol = 0;
            }
            var lenMin1 = this.lineMap.length - 1;
            this.currentTokens = this.tokensByLine[this.line];
            while((this.currentTokens.length == 0) && (this.line < lenMin1)) {
                this.line++;
                this.currentTokens = this.tokensByLine[this.line];
                tmpCol = 0;
            }
            if(this.line <= lenMin1) {
                while((this.currentTokenIndex < this.currentTokens.length) && (tmpCol > this.currentTokens[this.currentTokenIndex].limChar)) {
                    this.currentTokenIndex++;
                }
                if(this.currentTokenIndex < this.currentTokens.length) {
                    this.col = this.currentTokens[this.currentTokenIndex].minChar;
                    return this.col + this.lineMap[this.line];
                }
            }
            return -1;
        };
        SavedTokens.prototype.lastTokenLimChar = function () {
            if(this.prevSavedToken !== null) {
                return this.prevSavedToken.limChar;
            } else {
                return 0;
            }
        };
        SavedTokens.prototype.lastTokenHadNewline = function () {
            return this.prevLine != this.startLine;
        };
        SavedTokens.prototype.pushComment = function (comment) {
            this.commentStack.push(comment);
        };
        SavedTokens.prototype.getComments = function () {
            var stack = this.commentStack;
            this.commentStack = [];
            return stack;
        };
        SavedTokens.prototype.getCommentsForLine = function (line) {
            var comments = null;
            while((this.commentStack.length > 0) && (this.commentStack[0].line == line)) {
                if(comments == null) {
                    comments = [
                        this.commentStack.shift()
                    ];
                } else {
                    comments = comments.concat([
                        this.commentStack.shift()
                    ]);
                }
            }
            return comments;
        };
        SavedTokens.prototype.resetComments = function () {
            this.commentStack = [];
        };
        SavedTokens.prototype.setSourceText = function (newSrc, textMode) {
        };
        SavedTokens.prototype.setErrorHandler = function (reportError) {
        };
        SavedTokens.prototype.getLookAheadToken = function () {
            throw new Error("Invalid operation.");
        };
        return SavedTokens;
    })();
    TypeScript.SavedTokens = SavedTokens;    
    var Scanner = (function () {
        function Scanner() {
            this.prevLine = 1;
            this.line = 1;
            this.col = 0;
            this.pos = 0;
            this.startPos = 0;
            this.len = 0;
            this.lineMap = [];
            this.ch = TypeScript.LexEOF;
            this.lexState = LexState.Start;
            this.mode = LexMode.File;
            this.scanComments = true;
            this.interveningWhitespace = false;
            this.interveningWhitespacePos = 0;
            this.leftCurlyCount = 0;
            this.rightCurlyCount = 0;
            this.commentStack = new Array();
            this.saveScan = null;
            this.seenUnicodeChar = false;
            this.seenUnicodeCharInComment = false;
            this.prevTok = TypeScript.staticTokens[TypeScript.TokenID.EndOfFile];
            this.startCol = this.col;
            this.startLine = this.line;
            this.lineMap[1] = 0;
            if(!TypeScript.LexKeywordTable) {
                LexInitialize();
            }
        }
        Scanner.prototype.previousToken = function () {
            return this.prevTok;
        };
        Scanner.prototype.setSourceText = function (newSrc, textMode) {
            this.mode = textMode;
            this.scanComments = (this.mode === LexMode.Line);
            this.pos = 0;
            this.interveningWhitespacePos = 0;
            this.startPos = 0;
            this.line = 1;
            this.col = 0;
            this.startCol = this.col;
            this.startLine = this.line;
            this.len = 0;
            this.src = newSrc.getText(0, newSrc.getLength());
            this.len = this.src.length;
            this.lineMap = [];
            this.lineMap[1] = 0;
            this.commentStack = [];
            this.leftCurlyCount = 0;
            this.rightCurlyCount = 0;
            this.seenUnicodeChar = false;
            this.seenUnicodeCharInComment = false;
        };
        Scanner.prototype.setErrorHandler = function (reportError) {
            this.reportError = reportError;
        };
        Scanner.prototype.setSaveScan = function (savedTokens) {
            this.saveScan = savedTokens;
        };
        Scanner.prototype.setText = function (newSrc, textMode) {
            this.setSourceText(new StringSourceText(newSrc), textMode);
        };
        Scanner.prototype.setScanComments = function (value) {
            this.scanComments = value;
        };
        Scanner.prototype.getLexState = function () {
            return this.lexState;
        };
        Scanner.prototype.tokenStart = function () {
            this.startPos = this.pos;
            this.startLine = this.line;
            this.startCol = this.col;
            this.interveningWhitespace = false;
        };
        Scanner.prototype.peekChar = function () {
            if(this.pos < this.len) {
                return this.src.charCodeAt(this.pos);
            } else {
                return TypeScript.LexEOF;
            }
        };
        Scanner.prototype.peekCharAt = function (index) {
            if(index < this.len) {
                return this.src.charCodeAt(index);
            } else {
                return TypeScript.LexEOF;
            }
        };
        Scanner.prototype.IsHexDigit = function (c) {
            return ((c >= TypeScript.LexCode_0) && (c <= TypeScript.LexCode_9)) || ((c >= TypeScript.LexCode_A) && (c <= TypeScript.LexCode_F)) || ((c >= TypeScript.LexCode_a) && (c <= TypeScript.LexCode_f));
        };
        Scanner.prototype.IsOctalDigit = function (c) {
            return ((c >= TypeScript.LexCode_0) && (c <= TypeScript.LexCode_7)) || ((c >= TypeScript.LexCode_a) && (c <= TypeScript.LexCode_f));
        };
        Scanner.prototype.scanHexDigits = function () {
            var atLeastOneDigit = false;
            for(; ; ) {
                if(this.IsHexDigit(this.ch)) {
                    this.nextChar();
                    atLeastOneDigit = true;
                } else {
                    if(atLeastOneDigit) {
                        return new TypeScript.NumberLiteralToken(parseInt(this.src.substring(this.startPos, this.pos)));
                    } else {
                        return null;
                    }
                }
            }
        };
        Scanner.prototype.scanOctalDigits = function () {
            var atLeastOneDigit = false;
            for(; ; ) {
                if(this.IsOctalDigit(this.ch)) {
                    this.nextChar();
                    atLeastOneDigit = true;
                } else {
                    if(atLeastOneDigit) {
                        return new TypeScript.NumberLiteralToken(parseInt(this.src.substring(this.startPos, this.pos)));
                    } else {
                        return null;
                    }
                }
            }
        };
        Scanner.prototype.scanDecimalNumber = function (state) {
            var atLeastOneDigit = false;
            var svPos = this.pos;
            var svCol = this.col;
            for(; ; ) {
                if(LexIsDigit(this.ch)) {
                    atLeastOneDigit = true;
                    if(this.ch != TypeScript.LexCode_0 && state == NumberScanState.InEmptyFraction) {
                        state = NumberScanState.InFraction;
                    }
                    this.nextChar();
                } else if(this.ch == TypeScript.LexCodeDOT) {
                    if(state == NumberScanState.Start) {
                        this.nextChar();
                        state = NumberScanState.InEmptyFraction;
                    } else {
                        if(atLeastOneDigit) {
                            return new TypeScript.NumberLiteralToken(parseFloat(this.src.substring(this.startPos, this.pos)), state == NumberScanState.InEmptyFraction);
                        } else {
                            this.pos = svPos;
                            this.col = svCol;
                            return null;
                        }
                    }
                } else if((this.ch == TypeScript.LexCode_e) || (this.ch == TypeScript.LexCode_E)) {
                    if(state == NumberScanState.Start) {
                        if(atLeastOneDigit) {
                            atLeastOneDigit = false;
                            this.nextChar();
                            state = NumberScanState.InExponent;
                        } else {
                            this.pos = svPos;
                            this.col = svCol;
                            return null;
                        }
                    } else if(state == NumberScanState.InFraction || state == NumberScanState.InEmptyFraction) {
                        this.nextChar();
                        state = NumberScanState.InExponent;
                        atLeastOneDigit = false;
                    } else {
                        if(atLeastOneDigit) {
                            return new TypeScript.NumberLiteralToken(parseFloat(this.src.substring(this.startPos, this.pos)));
                        } else {
                            this.pos = svPos;
                            this.col = svCol;
                            return null;
                        }
                    }
                } else if((this.ch == TypeScript.LexCodePLS) || (this.ch == TypeScript.LexCodeMIN)) {
                    if(state == NumberScanState.InExponent) {
                        if(!atLeastOneDigit) {
                            this.nextChar();
                        } else {
                            this.pos = svPos;
                            this.col = svCol;
                            return null;
                        }
                    } else if(state == NumberScanState.InEmptyFraction || state == NumberScanState.InFraction) {
                        return new TypeScript.NumberLiteralToken(parseFloat(this.src.substring(this.startPos, this.pos)), state == NumberScanState.InEmptyFraction);
                    } else {
                        if(!atLeastOneDigit) {
                            this.pos = svPos;
                            this.col = svCol;
                            return null;
                        } else {
                            return new TypeScript.NumberLiteralToken(parseFloat(this.src.substring(this.startPos, this.pos)));
                        }
                    }
                } else {
                    if(!atLeastOneDigit) {
                        this.pos = svPos;
                        this.col = svCol;
                        return null;
                    } else {
                        return new TypeScript.NumberLiteralToken(parseFloat(this.src.substring(this.startPos, this.pos)), state == NumberScanState.InEmptyFraction);
                    }
                }
            }
        };
        Scanner.prototype.scanNumber = function () {
            if(this.peekChar() == TypeScript.LexCode_0) {
                switch(this.peekCharAt(this.pos + 1)) {
                    case TypeScript.LexCode_x:
                    case TypeScript.LexCode_X:
                        this.advanceChar(2);
                        return this.scanHexDigits();
                    case TypeScript.LexCode_8:
                    case TypeScript.LexCode_9:
                    case TypeScript.LexCodeDOT:
                        return this.scanDecimalNumber(NumberScanState.Start);
                    default:
                        return this.scanOctalDigits();
                }
            } else {
                return this.scanDecimalNumber(NumberScanState.Start);
            }
        };
        Scanner.prototype.scanFraction = function () {
            return this.scanDecimalNumber(NumberScanState.InFraction);
        };
        Scanner.prototype.newLine = function () {
            this.col = 0;
            if(this.mode == LexMode.File) {
                this.line++;
                this.lineMap[this.line] = this.pos + 1;
            }
        };
        Scanner.prototype.finishMultilineComment = function () {
            var ch2;
            this.lexState = LexState.InMultilineComment;
            while(this.pos < this.len) {
                if(this.ch == TypeScript.LexCodeMUL) {
                    ch2 = this.peekCharAt(this.pos + 1);
                    if(ch2 == TypeScript.LexCodeSLH) {
                        this.advanceChar(2);
                        if(this.mode == LexMode.File) {
                            this.tokenStart();
                        }
                        this.lexState = LexState.Start;
                        return true;
                    }
                } else if(this.ch == TypeScript.LexCodeNWL) {
                    this.newLine();
                    if(this.mode == LexMode.Line) {
                        this.nextChar();
                        return false;
                    }
                } else if(this.ch >= TypeScript.LexCodeASCIIChars) {
                    this.seenUnicodeCharInComment = true;
                }
                this.nextChar();
            }
            return false;
        };
        Scanner.prototype.pushComment = function (comment) {
            this.commentStack.push(comment);
        };
        Scanner.prototype.getComments = function () {
            var stack = this.commentStack;
            this.commentStack = [];
            return stack;
        };
        Scanner.prototype.getCommentsForLine = function (line) {
            var comments = null;
            while((this.commentStack.length > 0) && (this.commentStack[0].line == line)) {
                if(comments == null) {
                    comments = [
                        this.commentStack.shift()
                    ];
                } else {
                    comments = comments.concat([
                        this.commentStack.shift()
                    ]);
                }
            }
            return comments;
        };
        Scanner.prototype.resetComments = function () {
            this.commentStack = [];
        };
        Scanner.prototype.endsLine = function (c) {
            return (c == TypeScript.LexCodeNWL) || (c == TypeScript.LexCodeRET) || (c == TypeScript.LexCodeLS) || (c == TypeScript.LexCodePS);
        };
        Scanner.prototype.finishSinglelineComment = function () {
            while(this.pos < this.len) {
                if(this.endsLine(this.ch)) {
                    break;
                }
                if(this.ch >= TypeScript.LexCodeASCIIChars) {
                    this.seenUnicodeCharInComment = true;
                }
                this.nextChar();
            }
            if(this.mode == LexMode.File) {
                this.tokenStart();
            }
        };
        Scanner.prototype.tokenText = function () {
            return this.src.substring(this.startPos, this.pos);
        };
        Scanner.prototype.findClosingSLH = function () {
            var index = this.pos;
            var ch2 = this.src.charCodeAt(index);
            var prevCh = 0;
            var liveEsc = false;
            while(!this.endsLine(ch2) && (index < this.len)) {
                if((ch2 == TypeScript.LexCodeSLH) && (!liveEsc)) {
                    return index;
                }
                prevCh = ch2;
                index++;
                if(liveEsc) {
                    liveEsc = false;
                } else {
                    liveEsc = (prevCh == TypeScript.LexCodeBSL);
                }
                ch2 = this.src.charCodeAt(index);
            }
            return -1;
        };
        Scanner.prototype.speculateRegex = function () {
            if(TypeScript.noRegexTable[this.prevTok.tokenId] != undefined) {
                return null;
            }
            var svPos = this.pos;
            var svCol = this.col;
            var index = this.findClosingSLH();
            if(index > 0) {
                var pattern = this.src.substring(svPos, index);
                var flags = "";
                this.pos = index + 1;
                this.ch = this.peekChar();
                var flagsStart = this.pos;
                while((this.ch == TypeScript.LexCode_i) || (this.ch == TypeScript.LexCode_g) || (this.ch == TypeScript.LexCode_m)) {
                    this.nextChar();
                }
                if((this.pos - flagsStart) > 3) {
                    return null;
                } else {
                    flags = this.src.substring(flagsStart, this.pos);
                }
                var regex = undefined;
                try  {
                    regex = new RegExp(pattern, flags);
                } catch (regexException) {
                }
                if(regex) {
                    this.col = svCol + (this.pos - this.startPos);
                    return new TypeScript.RegularExpressionLiteralToken(regex);
                }
            }
            this.pos = svPos;
            this.col = svCol;
            return null;
        };
        Scanner.prototype.lastTokenHadNewline = function () {
            return this.prevLine != this.startLine;
        };
        Scanner.prototype.lastTokenLimChar = function () {
            return this.interveningWhitespace ? this.interveningWhitespacePos : this.startPos;
        };
        Scanner.prototype.advanceChar = function (amt) {
            this.pos += amt;
            this.col += amt;
            this.ch = this.peekChar();
        };
        Scanner.prototype.nextChar = function () {
            this.pos++;
            this.col++;
            this.ch = this.peekChar();
        };
        Scanner.prototype.getLookAheadToken = function () {
            var prevLine = this.prevLine;
            var line = this.line;
            var col = this.col;
            var pos = this.pos;
            var startPos = this.startPos;
            var startCol = this.startCol;
            var startLine = this.startLine;
            var ch = this.ch;
            var prevTok = this.prevTok;
            var lexState = this.lexState;
            var interveningWhitespace = this.interveningWhitespace;
            var interveningWhitespacePos = this.interveningWhitespacePos;
            var leftCurlyCount = this.leftCurlyCount;
            var rightCurlyCount = this.rightCurlyCount;
            var seenUnicodeChar = this.seenUnicodeChar;
            var seenUnicodeCharInComment = this.seenUnicodeCharInComment;
            var commentStackLength = this.commentStack.length;
            var lookAheadToken = this.scan();
            this.prevLine = prevLine;
            this.line = line;
            this.col = col;
            this.pos = pos;
            this.startPos = startPos;
            this.startCol = startCol;
            this.startLine = startLine;
            this.ch = ch;
            this.prevTok = prevTok;
            this.lexState = lexState;
            this.interveningWhitespace = interveningWhitespace;
            this.interveningWhitespacePos = interveningWhitespacePos;
            this.leftCurlyCount = leftCurlyCount;
            this.rightCurlyCount = rightCurlyCount;
            this.seenUnicodeChar = seenUnicodeChar;
            this.seenUnicodeCharInComment = seenUnicodeCharInComment;
            this.commentStack.length = commentStackLength;
            return lookAheadToken;
        };
        Scanner.prototype.scanInLine = function () {
            if((this.lexState == LexState.InMultilineComment) && (this.scanComments)) {
                this.ch = this.peekChar();
                var commentLine = this.line;
                this.finishMultilineComment();
                if(this.startPos < this.pos) {
                    var commentText = this.src.substring(this.startPos, this.pos);
                    this.tokenStart();
                    return new TypeScript.CommentToken(TypeScript.TokenID.Comment, commentText, true, this.startPos, commentLine, true);
                } else {
                    return TypeScript.staticTokens[TypeScript.TokenID.EndOfFile];
                }
            } else if(this.lexState == LexState.InMultilineSingleQuoteString && this.pos < this.len) {
                this.ch = TypeScript.LexCodeAPO;
                this.lexState = LexState.Start;
                return this.scanStringConstant();
            } else if(this.lexState == LexState.InMultilineDoubleQuoteString && this.pos < this.len) {
                this.ch = TypeScript.LexCodeQUO;
                this.lexState = LexState.Start;
                return this.scanStringConstant();
            }
            this.prevLine = this.line;
            var prevTok = this.innerScan();
            if(prevTok.tokenId != TypeScript.TokenID.Whitespace) {
                this.prevTok = prevTok;
            }
            return prevTok;
        };
        Scanner.prototype.scan = function () {
            this.prevLine = this.line;
            this.prevTok = this.innerScan();
            if(this.saveScan) {
                this.saveScan.addToken(this.prevTok, this);
            }
            return this.prevTok;
        };
        Scanner.prototype.isValidUnicodeIdentifierChar = function () {
            var valid = LexIsUnicodeIdStart(this.ch) || LexIsUnicodeDigit(this.ch);
            this.seenUnicodeChar = this.seenUnicodeChar || valid;
            return valid;
        };
        Scanner.prototype.scanStringConstant = function () {
            var endCode = this.ch;
            this.nextChar();
            scanStringConstantLoop:
for(; ; ) {
                switch(this.ch) {
                    case TypeScript.LexEOF:
                        this.reportScannerError("Unterminated string constant");
                        break scanStringConstantLoop;
                    case TypeScript.LexCodeLS:
                    case TypeScript.LexCodePS:
                        this.seenUnicodeChar = true;
                    case TypeScript.LexCodeRET:
                    case TypeScript.LexCodeNWL:
                        this.reportScannerError("Unterminated string constant");
                        break scanStringConstantLoop;
                    case TypeScript.LexCodeAPO:
                    case TypeScript.LexCodeQUO:
                        if(this.ch == endCode) {
                            this.nextChar();
                            break scanStringConstantLoop;
                        }
                        break;
                    case TypeScript.LexCodeBSL:
                        this.nextChar();
                        switch(this.ch) {
                            case TypeScript.LexCodeAPO:
                            case TypeScript.LexCodeQUO:
                            case TypeScript.LexCodeBSL:
                                this.nextChar();
                                continue scanStringConstantLoop;
                            case TypeScript.LexCodeLS:
                            case TypeScript.LexCodePS:
                                this.seenUnicodeChar = true;
                            case TypeScript.LexCodeRET:
                            case TypeScript.LexCodeNWL:
                                if(this.ch == TypeScript.LexCodeRET && this.peekCharAt(this.pos + 1) == TypeScript.LexCodeNWL) {
                                    this.nextChar();
                                }
                                this.nextChar();
                                this.newLine();
                                if(this.mode == LexMode.Line) {
                                    this.lexState = endCode == TypeScript.LexCodeAPO ? LexState.InMultilineSingleQuoteString : LexState.InMultilineDoubleQuoteString;
                                    break scanStringConstantLoop;
                                }
                                break;
                            case TypeScript.LexCode_x:
                            case TypeScript.LexCode_u:
                                var expectedHexDigits = this.ch == TypeScript.LexCode_x ? 2 : 4;
                                this.nextChar();
                                for(var i = 0; i < expectedHexDigits; i++) {
                                    if(this.IsHexDigit(this.ch)) {
                                        this.nextChar();
                                    } else {
                                        this.reportScannerError("Invalid Unicode escape sequence");
                                        break;
                                    }
                                }
                                continue scanStringConstantLoop;
                        }
                        break;
                }
                if(this.ch >= TypeScript.LexCodeASCIIChars) {
                    this.seenUnicodeChar = true;
                }
                this.nextChar();
            }
            return new TypeScript.StringLiteralToken(this.src.substring(this.startPos, this.pos));
        };
        Scanner.prototype.scanIdentifier = function () {
            var hasEscape = false;
            var isFirstChar = (this.ch == TypeScript.LexCodeBSL);
            var hasUnicode = false;
            for(; ; ) {
                while(lexIdStartTable[this.ch] || LexIsDigit(this.ch) || (this.ch >= TypeScript.LexCodeASCIIChars && this.isValidUnicodeIdentifierChar())) {
                    this.nextChar();
                }
                if(this.ch == TypeScript.LexCodeBSL) {
                    this.nextChar();
                    if(this.ch == TypeScript.LexCode_u) {
                        this.nextChar();
                        for(var h = 0; h < 4; h++) {
                            if(this.IsHexDigit(this.ch)) {
                                this.nextChar();
                            } else {
                                this.reportScannerError("Invalid Unicode escape sequence");
                                return TypeScript.staticTokens[TypeScript.TokenID.Error];
                            }
                        }
                        var hexChar = parseInt(this.src.substring(this.pos - 4, this.pos), 16);
                        if(lexIdStartTable[hexChar] || (!isFirstChar && LexIsDigit(hexChar)) || (hexChar >= TypeScript.LexCodeASCIIChars && (LexIsUnicodeIdStart(hexChar) || (!isFirstChar && LexIsUnicodeDigit(hexChar))))) {
                        } else {
                            this.reportScannerError("Invalid identifier character");
                            return TypeScript.staticTokens[TypeScript.TokenID.Error];
                        }
                        hasEscape = true;
                        isFirstChar = false;
                        continue;
                    }
                    this.reportScannerError("Invalid Unicode escape sequence");
                    return TypeScript.staticTokens[TypeScript.TokenID.Error];
                }
                break;
            }
            var id;
            var text = this.src.substring(this.startPos, this.pos);
            if(!hasEscape && (id = TypeScript.LexKeywordTable.lookup(text)) != null) {
                return TypeScript.staticTokens[id];
            } else {
                return new TypeScript.IdentifierToken(text, hasEscape);
            }
        };
        Scanner.prototype.innerScan = function () {
            var rtok;
            this.tokenStart();
            this.ch = this.peekChar();
            start:
while(this.pos < this.len) {
                if(lexIdStartTable[this.ch] || this.ch == TypeScript.LexCodeBSL || (this.ch >= TypeScript.LexCodeASCIIChars && LexIsUnicodeIdStart(this.ch))) {
                    return this.scanIdentifier();
                } else if(this.ch == TypeScript.LexCodeSpace) {
                    if(!this.interveningWhitespace) {
                        this.interveningWhitespacePos = this.pos;
                    }
                    do {
                        this.nextChar();
                    }while(this.ch == TypeScript.LexCodeSpace);
                    if(this.mode == LexMode.Line) {
                        var whitespaceText = this.src.substring(this.startPos, this.pos);
                        return new TypeScript.WhitespaceToken(TypeScript.TokenID.Whitespace, whitespaceText);
                    } else {
                        this.tokenStart();
                        this.interveningWhitespace = true;
                    }
                } else if(this.ch == TypeScript.LexCodeSLH) {
                    this.nextChar();
                    var commentText;
                    if(this.ch == TypeScript.LexCodeSLH) {
                        if(!this.interveningWhitespace) {
                            this.interveningWhitespacePos = this.pos - 1;
                        }
                        var commentStartPos = this.pos - 1;
                        var commentStartLine = this.line;
                        this.finishSinglelineComment();
                        var commentText = this.src.substring(commentStartPos, this.pos);
                        var commentToken = new TypeScript.CommentToken(TypeScript.TokenID.Comment, commentText, false, commentStartPos, commentStartLine, false);
                        if(this.scanComments) {
                            this.startPos = commentStartPos;
                            return commentToken;
                        } else {
                            this.pushComment(commentToken);
                        }
                        this.interveningWhitespace = true;
                    } else if(this.ch == TypeScript.LexCodeMUL) {
                        if(!this.interveningWhitespace) {
                            this.interveningWhitespacePos = this.pos - 1;
                        }
                        var commentStartPos = this.pos - 1;
                        var commentStartLine = this.line;
                        this.nextChar();
                        this.finishMultilineComment();
                        var commentText = this.src.substring(commentStartPos, this.pos);
                        var endsLine = this.endsLine(this.peekChar());
                        var commentToken = new TypeScript.CommentToken(TypeScript.TokenID.Comment, commentText, true, commentStartPos, commentStartLine, endsLine);
                        if(this.scanComments) {
                            this.startPos = commentStartPos;
                            return commentToken;
                        } else {
                            this.pushComment(commentToken);
                        }
                        this.interveningWhitespace = true;
                    } else {
                        var regexTok = this.speculateRegex();
                        if(regexTok) {
                            return regexTok;
                        } else {
                            if(this.peekCharAt(this.pos) == TypeScript.LexCodeEQ) {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.SlashEquals];
                            } else {
                                return TypeScript.staticTokens[TypeScript.TokenID.Slash];
                            }
                        }
                    }
                } else if(this.ch == TypeScript.LexCodeSMC) {
                    this.nextChar();
                    return TypeScript.staticTokens[TypeScript.TokenID.Semicolon];
                } else if((this.ch == TypeScript.LexCodeAPO) || (this.ch == TypeScript.LexCodeQUO)) {
                    return this.scanStringConstant();
                } else if(autoToken[this.ch]) {
                    var atok = autoToken[this.ch];
                    if(atok.tokenId == TypeScript.TokenID.OpenBrace) {
                        this.leftCurlyCount++;
                    } else if(atok.tokenId == TypeScript.TokenID.CloseBrace) {
                        this.rightCurlyCount++;
                    }
                    this.nextChar();
                    return atok;
                } else if((this.ch >= TypeScript.LexCode_0) && (this.ch <= TypeScript.LexCode_9)) {
                    rtok = this.scanNumber();
                    if(rtok) {
                        return rtok;
                    } else {
                        this.nextChar();
                        return TypeScript.staticTokens[TypeScript.TokenID.Error];
                    }
                } else {
                    switch(this.ch) {
                        case TypeScript.LexCodeTAB:
                        case TypeScript.LexCodeVTAB:
                            if(!this.interveningWhitespace) {
                                this.interveningWhitespacePos = this.pos;
                            }
                            if(this.mode == LexMode.Line) {
                                do {
                                    this.nextChar();
                                }while((this.ch == TypeScript.LexCodeSpace) || (this.ch == 9));
                                var wsText = this.src.substring(this.startPos, this.pos);
                                return new TypeScript.WhitespaceToken(TypeScript.TokenID.Whitespace, wsText);
                            } else {
                                this.interveningWhitespace = true;
                            }
                        case 255:
                        case 254:
                        case 239:
                        case 187:
                        case 191:
                        case TypeScript.LexCodeLS:
                        case TypeScript.LexCodePS:
                        case TypeScript.LexCodeNWL:
                        case TypeScript.LexCodeRET:
                            if(this.ch == TypeScript.LexCodeNWL) {
                                this.newLine();
                                if(this.mode == LexMode.Line) {
                                    return TypeScript.staticTokens[TypeScript.TokenID.EndOfFile];
                                }
                            }
                            if(!this.interveningWhitespace) {
                                this.interveningWhitespacePos = this.pos;
                            }
                            this.nextChar();
                            this.tokenStart();
                            this.interveningWhitespace = true;
                            break;
                        case TypeScript.LexCodeDOT: {
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeDOT) {
                                if(this.peekCharAt(this.pos + 2) == TypeScript.LexCodeDOT) {
                                    this.advanceChar(3);
                                    return TypeScript.staticTokens[TypeScript.TokenID.DotDotDot];
                                } else {
                                    this.nextChar();
                                    return TypeScript.staticTokens[TypeScript.TokenID.Dot];
                                }
                            } else {
                                this.nextChar();
                                rtok = this.scanFraction();
                                if(rtok) {
                                    return rtok;
                                } else {
                                    return TypeScript.staticTokens[TypeScript.TokenID.Dot];
                                }
                            }
                        }
                        case TypeScript.LexCodeEQ:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                if(this.peekCharAt(this.pos + 2) == TypeScript.LexCodeEQ) {
                                    this.advanceChar(3);
                                    return TypeScript.staticTokens[TypeScript.TokenID.EqualsEqualsEquals];
                                } else {
                                    this.advanceChar(2);
                                    return TypeScript.staticTokens[TypeScript.TokenID.EqualsEquals];
                                }
                            } else if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeGT) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.EqualsGreaterThan];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Equals];
                            }
                        case TypeScript.LexCodeBNG:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                if(this.peekCharAt(this.pos + 2) == TypeScript.LexCodeEQ) {
                                    this.advanceChar(3);
                                    return TypeScript.staticTokens[TypeScript.TokenID.ExclamationEqualsEquals];
                                } else {
                                    this.advanceChar(2);
                                    return TypeScript.staticTokens[TypeScript.TokenID.ExclamationEquals];
                                }
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Exclamation];
                            }
                        case TypeScript.LexCodePLS:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.PlusEquals];
                            } else if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodePLS) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.PlusPlus];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Plus];
                            }
                        case TypeScript.LexCodeMIN:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.MinusEquals];
                            } else if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeMIN) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.MinusMinus];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Minus];
                            }
                        case TypeScript.LexCodeMUL:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.AsteriskEquals];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Asterisk];
                            }
                        case TypeScript.LexCodePCT:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.PercentEquals];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Percent];
                            }
                        case TypeScript.LexCodeLT:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeLT) {
                                if(this.peekCharAt(this.pos + 2) == TypeScript.LexCodeEQ) {
                                    this.advanceChar(3);
                                    return TypeScript.staticTokens[TypeScript.TokenID.LessThanLessThanEquals];
                                } else {
                                    this.advanceChar(2);
                                    return TypeScript.staticTokens[TypeScript.TokenID.LessThanLessThan];
                                }
                            } else if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.LessThanEquals];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.LessThan];
                            }
                        case TypeScript.LexCodeGT:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeGT) {
                                if(this.peekCharAt(this.pos + 2) == TypeScript.LexCodeEQ) {
                                    this.advanceChar(3);
                                    return TypeScript.staticTokens[TypeScript.TokenID.GreaterThanGreaterThanEquals];
                                } else if(this.peekCharAt(this.pos + 2) == TypeScript.LexCodeGT) {
                                    if(this.peekCharAt(this.pos + 3) == TypeScript.LexCodeEQ) {
                                        this.advanceChar(4);
                                        return TypeScript.staticTokens[TypeScript.TokenID.GreaterThanGreaterThanGreaterThanEquals];
                                    } else {
                                        this.advanceChar(3);
                                        return TypeScript.staticTokens[TypeScript.TokenID.GreaterThanGreaterThanGreaterThan];
                                    }
                                } else {
                                    this.advanceChar(2);
                                    return TypeScript.staticTokens[TypeScript.TokenID.GreaterThanGreaterThan];
                                }
                            } else if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.GreaterThanEquals];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.GreaterThan];
                            }
                        case TypeScript.LexCodeXOR:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.CaretEquals];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Caret];
                            }
                        case TypeScript.LexCodeBAR:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.BarEquals];
                            } else if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeBAR) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.BarBar];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.Bar];
                            }
                        case TypeScript.LexCodeAMP:
                            if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeEQ) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.AmpersandEquals];
                            } else if(this.peekCharAt(this.pos + 1) == TypeScript.LexCodeAMP) {
                                this.advanceChar(2);
                                return TypeScript.staticTokens[TypeScript.TokenID.AmpersandAmpersand];
                            } else {
                                this.nextChar();
                                return TypeScript.staticTokens[TypeScript.TokenID.And];
                            }
                        default:
                            this.reportScannerError("Invalid character");
                            this.nextChar();
                            continue start;
                    }
                }
            }
            return TypeScript.staticTokens[TypeScript.TokenID.EndOfFile];
        };
        Scanner.prototype.reportScannerError = function (message) {
            if(this.reportError) {
                this.reportError(message);
            }
        };
        return Scanner;
    })();
    TypeScript.Scanner = Scanner;    
    function convertTokToIDName(tok) {
        return convertTokToIDBase(tok, true, false);
    }
    TypeScript.convertTokToIDName = convertTokToIDName;
    function convertTokToID(tok, strictMode) {
        return convertTokToIDBase(tok, false, strictMode);
    }
    TypeScript.convertTokToID = convertTokToID;
    function convertTokToIDBase(tok, identifierName, strictMode) {
        if(tok.tokenId <= TypeScript.TokenID.LimKeyword) {
            var tokInfo = TypeScript.lookupToken(tok.tokenId);
            if(tokInfo != undefined) {
                var resFlags = TypeScript.Reservation.Javascript | TypeScript.Reservation.JavascriptFuture;
                if(strictMode) {
                    resFlags |= TypeScript.Reservation.JavascriptFutureStrict;
                }
                if(identifierName || !TypeScript.hasFlag(tokInfo.reservation, resFlags)) {
                    return true;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
    function getLineNumberFromPosition(lineMap, position) {
        if(position === -1) {
            return 0;
        }
        var min = 0;
        var max = lineMap.length - 1;
        while(min < max) {
            var med = (min + max) >> 1;
            if(position < lineMap[med]) {
                max = med - 1;
            } else if(position < lineMap[med + 1]) {
                min = max = med;
            } else {
                min = med + 1;
            }
        }
        return min;
    }
    TypeScript.getLineNumberFromPosition = getLineNumberFromPosition;
    function getSourceLineColFromMap(lineCol, minChar, lineMap) {
        var line = getLineNumberFromPosition(lineMap, minChar);
        if(line > 0) {
            lineCol.line = line;
            lineCol.col = (minChar - lineMap[line]);
        }
    }
    TypeScript.getSourceLineColFromMap = getSourceLineColFromMap;
    function getLineColumnFromPosition(script, position) {
        var result = {
            line: -1,
            col: -1
        };
        getSourceLineColFromMap(result, position, script.locationInfo.lineMap);
        if(result.col >= 0) {
            result.col++;
        }
        return result;
    }
    TypeScript.getLineColumnFromPosition = getLineColumnFromPosition;
    function getPositionFromLineColumn(script, line, column) {
        return script.locationInfo.lineMap[line] + (column - 1);
    }
    TypeScript.getPositionFromLineColumn = getPositionFromLineColumn;
    function isPrimitiveTypeToken(token) {
        switch(token.tokenId) {
            case TypeScript.TokenID.Any:
            case TypeScript.TokenID.Bool:
            case TypeScript.TokenID.Number:
            case TypeScript.TokenID.String:
                return true;
        }
        return false;
    }
    TypeScript.isPrimitiveTypeToken = isPrimitiveTypeToken;
    function isModifier(token) {
        switch(token.tokenId) {
            case TypeScript.TokenID.Public:
            case TypeScript.TokenID.Private:
            case TypeScript.TokenID.Static:
                return true;
        }
        return false;
    }
    TypeScript.isModifier = isModifier;
})(TypeScript || (TypeScript = {}));
