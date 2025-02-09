(defpackage #:org.shirakumo.fraf.trial.space.bsp-test
  (:use #:cl #:parachute #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames
   (#:space #:org.shirakumo.fraf.trial.space)
   (#:bsp #:org.shirakumo.fraf.trial.space.bsp))
  (:export
   #:test))

(in-package #:org.shirakumo.fraf.trial.space.bsp-test)

(defparameter *model-paths-alist*
  '((:funny-house . "./test-mesh/funny-house.obj")
    (:level . "./test-mesh/level.obj")))

(defparameter *model-cache* (make-hash-table))

(defparameter *bsp-cache* (make-hash-table))

(defun wavefront-obj-to-mesh (obj)
  (let* ((in-verts (org.shirakumo.fraf.wavefront:vertices obj))
         (in-faces (org.shirakumo.fraf.wavefront:faces
                    (slot-value obj 'org.shirakumo.fraf.wavefront::current)))
         (out-verts (make-array (* 3 (/ (length in-verts) 4)) :element-type 'single-float))
         (out-faces (make-array (* 3 (length in-faces)) :element-type '(unsigned-byte 32))))
    (loop for ii below (/ (length out-verts) 3)
          do (setf (aref out-verts (+ 0 (* ii 3))) (aref in-verts (+ 0 (* ii 4))))
          do (setf (aref out-verts (+ 1 (* ii 3))) (aref in-verts (+ 1 (* ii 4))))
          do (setf (aref out-verts (+ 2 (* ii 3))) (aref in-verts (+ 2 (* ii 4)))))
    (loop for f across in-faces
          for ii from 0
          for face-verts = (org.shirakumo.fraf.wavefront:vertices f)
          do (assert (= 3 (length face-verts)))
             (setf (aref out-faces (+ 0 (* 3 ii))) (aref face-verts 0))
             (setf (aref out-faces (+ 1 (* 3 ii))) (aref face-verts 1))
             (setf (aref out-faces (+ 2 (* 3 ii))) (aref face-verts 2)))
    (space:mesh out-verts out-faces)))

(defun get-model (name)
  "Get a model by name - see *MODEL-PATHS-ALIST*. Repeated calls will
be cached so multiple tests aren't re-loading the same asset, see
*MODEL-CACHE*"
  (when (not (gethash name *model-cache*))
    (print (assoc name *model-paths-alist*))
    (setf (gethash name *model-cache*)
          (wavefront-obj-to-mesh
           (org.shirakumo.fraf.wavefront:parse (with-open-file (s (cdr (assoc name *model-paths-alist*))) (uiop:read-file-string s))))))
  (gethash name *model-cache*))

(defun build-bsp-from-meshes (meshes)
  "Build a BSP from the model named NAME."
  (let ((bsp (bsp:make-bsp)))
    (loop for mesh in meshes do (space:enter (bsp:make-mesh-input-data :mesh mesh :user-data "test-user-data") bsp))
    (space:reoptimize bsp)
    bsp))

(define-test bsp)

(defparameter *ray-tests*
  '((:level
     ((-1.0000801 7.979286 1.8526587) -9.699483 7.299576 -4.9108458 5.6866093
      8.501738 7.051344)
     ((-1.0000801 2.1509879 0.34548378) -9.698202 5.793503 6.5665264 3.2335644
      0.3780632 -2.6824903)
     ((9.999961 2.3745735 -2.4770052) 6.9214077 2.0709438 -2.6408482 9.6798935
      2.3430061 -2.4940395)
     (8.174362 9.29641 6.98971 -5.4344845 16.75728 -1.6521406)
     ((-4.979659 10.50004 -1.2120886) -7.014549 5.955284 4.5460653 -3.730886
      13.28907 -4.7457576)
     ((-1.0000803 3.0961933 1.137006) -1.3230629 2.9960537 1.2533646 7.838108
      5.836444 -2.047062)
     ((-7.101342 11.5000725 -0.972613) -7.544372 11.917898 -0.59786797 -2.5884533
      7.243927 -4.789922)
     ((-9.918213e-5 3.4436226 -0.26631355) 7.586788 5.677614 8.118328 -6.6466045
      1.4865303 -7.6116943)
     (0.89139175 12.948528 2.1601248 5.882635 13.444969 -1.7927933)
     ((-1.3770622 11.500069 6.205099) -1.3290577 11.705479 6.5117455 -3.451693
      2.6227646 -7.0473576)
     (7.5791397 18.190592 -7.0810413 -0.051765442 19.236223 -0.61610985)
     ((-10.39661 11.500042 -10.285871) -4.573326 16.797588 -3.3686447 -14.785476
      7.50741 -15.499215)
     ((-1.0000815 9.2257595 6.959542) -8.464956 11.876251 14.774773 19.278275
      2.025692 -14.270558)
     ((-1.0000811 6.8471327 -2.2999115) -12.580347 7.8214626 -12.913322 8.197845
      6.073246 6.130066)
     ((11.99994 2.9596987 -16.668772) 13.956074 1.4392967 -18.52951 -9.558883
      19.716263 3.8386822)
     (17.829288 14.764496 19.838066 9.364691 18.800701 13.35025)
     (10.348644 17.055187 14.858814 14.832645 18.625298 19.581394)
     ((-15.064012 10.50004 8.572739) -17.733978 6.6503925 13.091896 -10.86947
      16.54787 1.4731026)
     (11.4468 5.904543 2.6872215 4.945612 15.58304 -12.751293)
     ((-5.6956053 10.50004 9.050633) -6.2703323 9.510853 10.398403 -3.7442303
      13.858633 4.4745445)
     (16.741108 3.9648266 -6.4866686 -7.3957253 16.22665 -13.902259)
     ((-18.751598 7.659218 -18.097595) -18.751598 7.659218 -18.097595 -4.2238617
      0.30429077 -15.596023)
     ((-13.666929 1.0000105 4.353588) 13.139351 9.330349 -11.161332 -16.041931
      0.26195335 5.7281914)
     (-2.9357243 7.890808 -17.945938 -12.699509 6.119623 -19.80951)
     ((-1.0262771 11.500047 -9.38265) -1.429676 16.320118 -5.573497 -0.6838703
      7.4087505 -12.615876)
     (0.27153015 15.660618 -14.300351 8.14827 16.834816 5.271717)
     (16.930431 8.486084 14.692478 5.552845 11.152096 -14.3777895)
     (2.2003784 10.901945 -12.542844 5.2190056 6.0646915 1.7291412)
     ((-1.3909111 0.85105133 8.960314) -1.3909111 0.85105133 8.960314 -9.3125725
      13.923016 -7.9002476)
     (19.943222 14.94717 14.878407 -4.753151 6.0219936 13.856243)
     ((-9.894371e-5 5.4402647 6.1275616) 3.247284 4.4452267 7.940773 -9.705644
      8.414162 0.7083664)
     (19.574898 17.493904 3.2750702 9.33622 10.215878 19.746265)
     ((8.192824 2.0016289 -23.999977) 11.945181 12.043608 14.864861 9.802555
      6.309557 -7.3272753)
     (12.78297 6.6848946 -12.715931 -0.15386963 14.804653 -2.6949883))
    (:funny-house
     ((5.533046 2.7873592 7.7201567) -8.268101 19.527609 -1.7840843 5.0999784 3.3126526 7.4219227)
     (1.7873907 3.7147236 -4.9357724 3.5395412 11.026258 -5.184276)
     ((-3.2788293 2.7873602 0.34559047) -3.0391192 14.654781 -1.401865 -3.1605935 8.640909 -0.5163336)
     ((-0.5630156 3.8631248 1.0) -2.5034666 8.213892 5.4056053 -0.16403961 2.968564 0.094163895)
     ((0.219372 0.78736 -2.1293056) 0.3397255 0.6000061 -2.0346355 -7.6075816 12.971554 -8.285995)
     (2.7428818 19.016922 -9.033625 8.372267 15.116215 0.9261179)
     (7.7064056 4.6327543 6.094198 0.77952576 10.664255 0.5721855)
     (7.4689293 15.231911 8.683765 -5.551584 10.414112 0.34249115)
     (-6.935208 2.4128704 1.2821503 -6.9811893 19.674036 7.5047016)
     ((3.0354197 0.78735995 1.1709542) 3.5560436 0.18850136 0.7441473 -7.1504903 12.503914 9.521351)
     (-2.5139546 13.0690155 -8.584509 -8.230057 8.4948845 -1.5131359)
     ((1.9864717 7.144528 0.79277134) 9.818136 1.3625479 9.047459 -5.331857 12.547522 -6.9208527)
     (8.468082 6.248646 9.219784 7.344372 0.35601616 3.8104172)
     ((6.774481 1.3441639 -1.8996265) 7.750597 1.3224363 -2.37185 -6.3941 1.6372852 4.4710426)
     ((4.791959 5.0593853 -3.704107) 0.512228 7.515957 -6.2449074 4.8668766 5.016382 -3.6596298)
     (7.56234 16.086193 -0.49179554 -4.565909 9.659336 -4.2072845)
     (2.4026775 12.854654 0.6470108 -1.8407011 10.557444 9.298021)
     ((-4.7029514 3.4223323 2.0400352) -2.3546672 8.122589 -7.540078 -5.786507 1.253519 6.460533)
     (-7.6778913 15.251022 5.3244 3.102684 7.957075 5.0030518)
     (7.1930904 9.626047 3.2399292 -7.893648 7.1259136 1.5363102)
     (6.254614 15.147114 -0.69920063 -7.127037 3.4059124 4.959448)
     (-8.544882 15.008732 -0.8648443 -1.336195 15.927294 8.906748)
     ((5.1747417 8.326993 1.0) 8.718594 10.061235 2.9293652 -1.8501492 4.889248 -2.824533)
     (4.03722 19.430635 7.232645 -4.576118 9.054176 -6.0214067)
     (-2.1859074 18.955193 -4.368446 -9.212019 9.991512 4.7262573)
     (-1.0968542 16.682625 -0.20671368 0.7971716 5.7674503 7.244465)
     ((2.6987357 2.78736 -4.173854) -2.9791808 3.7633276 -3.1779504 4.7670937 2.4318333 -4.536643)
     (-0.66049385 16.769926 0.4537487 5.1243 16.96685 -7.3176312)
     (-1.7923641 18.337755 -2.4058485 -4.4610667 19.549753 -7.2220516)
     (6.8414116 11.2022 1.5488129 -7.0434 7.4942875 7.513794)
     ((-2.7845216 2.7873611 -3.5756617) -9.91206 12.066896 -8.841484 -1.9443989 1.6935825 -2.954979)
     ((5.9370313 4.741484 1.0) 5.114794 12.356661 8.599594 6.4428616 0.05672264 -3.675177)
     ((0.7117765 4.5274925 -1.0) 0.28700066 5.9479885 -7.0667934 1.0623693 3.355072 4.007287)
     ((-4.6698446 2.7873602 -2.8034847) 7.2024574 16.154127 -0.4090519 -6.6258907 0.58509064 -3.1979847)))
  "A list of tests for ray intersection. These were generated randomly
  and each result visually checked in a 3D rendering of the test
  scene.

  The format of each test case is as follows:

  (RESULT? X0 Y0 Z0 X1 Y1 Z1)

  where X0 Y0 Z0, X1 Y1 Z1 are 2 points that construct a ray from P0,
  and where RESULT is an optional list of the form:

  (X Y Z)

  The result is the ray intersection. Some items don't have a
  result, in which case the ray didn't hit anything.")

(defparameter *aabb-tests*
  '((:funny-house
     (NIL 0.39234924 4.945747 -7.201767 1.0328517 1.8490033 1.9704223)
     (T -7.720115 2.64223 -6.302488 1.3329763 0.63230705 2.8767245)
     (NIL -6.6767144 4.5247946 8.839684 1.0702454 0.9785708 1.523377)
     (T -3.0728579 0.41075087 8.048496 1.251803 1.681042 0.52690107)
     (T 3.3295555 3.6007128 -5.2260423 0.78504527 2.5961144 2.215883)
     (T 0.802145 2.577335 -8.06004 1.3667958 1.6252804 1.555562)
     (T -1.247673 1.849781 -3.9685535 2.9389966 0.42861217 1.7347443)
     (T 4.8008327 4.2900963 5.882971 2.6696 2.112309 1.7709827)
     (T -0.6876898 0.24752092 1.833725 1.7100494 2.0052829 2.8520706)
     (T -3.4013414 -1.0716953 7.2770157 0.93470603 2.14196 1.559962)
     (T -3.217709 4.9124303 5.3052444 0.9192974 0.69513094 1.189611)
     (T 5.465124 3.4527621 7.914215 2.732979 2.5359926 2.9547808)
     (NIL 2.7900648 8.96295 2.3496895 0.8501515 2.8672714 0.8947922)
     (T -7.8060412 -1.5873189 4.1343384 2.604017 2.7898607 2.4220617)
     (NIL 0.25475693 -1.311698 -1.8695278 1.6637135 0.75790584 2.8673942)
     (NIL -2.2170615 9.677325 -3.480587 0.65886253 1.2476037 1.5770209)
     (NIL -9.034648 9.689806 9.408855 2.4683871 0.6580614 1.5782762)
     (T 8.479389 2.8007479 0.5725746 2.9551065 0.39641792 1.1526122)
     (NIL 9.553812 6.8719034 1.8903637 2.291907 0.5691842 1.0040293)
     (T 5.914998 3.1970325 0.7835579 2.4353495 1.8952138 1.2342839)
     (NIL 6.696472 -1.2285008 -0.39019585 0.3789886 0.5369568 1.7994826)
     (NIL -6.028335 9.303144 -4.8741937 0.35513905 2.8692722 2.7381694)
     (NIL -6.377747 8.4545 -4.790385 1.6142836 1.2073927 1.992367)
     (T 5.26038 6.725992 -8.5658245 0.32514444 2.6956134 2.3030317)
     (NIL -2.3905635 6.5636787 -1.2144661 0.9119225 2.6786866 0.7082653)
     (NIL 1.6858959 6.4993477 6.667694 1.0593889 1.9666913 1.6351926)
     (T -3.2703257 1.2163892 -0.62843513 1.4045911 0.30809236 0.8247592)
     (T 5.954073 1.2921247 -5.674927 2.941825 1.1132188 1.5930469)
     (T 2.3962193 4.1228952 2.404108 2.4325554 1.2039491 2.642683)
     (NIL 7.5108814 8.9797735 5.071459 1.979958 2.3364987 2.947432)
     (NIL -2.8854918 -0.90439844 -5.9775114 0.48695308 1.2540902 1.6249535)
     (NIL -3.9127564 8.37377 6.541376 1.6820085 2.3165214 0.69517183)
     (T 4.949832 2.1547604 2.1201057 1.2624693 1.0014863 0.5081247)
     (T -0.041065216 4.2944508 6.400118 2.6552384 1.8841763 2.0155966)
     (T -0.82166195 4.367893 1.9975071 1.6926425 2.0092504 0.46576408)
     (T 6.490118 6.6319427 -0.2875347 1.9756157 2.7752316 2.3739777)
     (T -0.18264961 2.2811775 -2.6686382 1.281216 2.7665355 1.8986256)
     (T 0.3273754 1.591435 -4.5917034 0.4259407 2.0798924 2.7995312)
     (NIL 1.8161392 5.5885215 -7.9014087 2.4658253 0.96046686 2.3662443)
     (T 4.7375584 8.874144 -4.380181 1.5195751 2.9391234 0.9721866)
     (NIL 0.07650185 -0.375587 9.66581 1.4372385 1.2381357 1.7460892)
     (NIL 1.0626774 5.741323 -5.7405114 2.9932687 1.5933068 2.5746646)
     (NIL -8.24353 8.80033 8.389824 0.3853213 0.8254937 1.8220382)
     (T -3.6665869 5.450035 6.2192535 0.5510467 1.8020899 1.68939)
     (NIL 5.043683 -0.8282752 8.154564 2.541556 0.45175362 0.47333756)
     (NIL 1.9094353 -1.0634813 9.471516 0.91621107 2.0782373 1.5507612)
     (T 5.1355057 2.4491053 -5.3133655 2.0494556 2.3461945 2.2681193)
     (T 1.0266418 8.589519 -2.0504332 0.6352708 2.0000236 2.8668103)
     (T 5.0766964 2.9742703 2.1664095 2.7983816 2.3271127 2.6235032)
     (NIL 9.934811 4.817962 1.2320633 2.2701826 2.3043873 2.098201)
     (T 0.9361 -0.012106895 -3.1819534 2.1005118 2.0055616 0.40873504)
     (NIL -7.95151 4.2308702 -2.701707 2.2961574 1.3301411 2.1177227)
     (NIL 7.3716946 -1.1765575 -4.6287227 0.5867107 2.4322472 1.5598927)
     (NIL -6.0502934 6.095249 9.499769 2.9913158 0.6107266 1.5380077)
     (NIL -4.1689873 -1.3705335 -0.14782906 1.3740091 1.9705215 1.117818)
     (NIL 4.4028835 -1.6032047 -6.36755 2.0964768 0.9370438 0.6196114)
     (T 4.955929 4.249799 -7.867627 1.408812 0.68128526 1.1567731)
     (T -2.7531695 -0.021495342 0.49922943 0.7767379 1.9776411 2.2484906)
     (NIL 5.5291157 -1.904582 0.2968769 1.7246776 0.48833036 0.71033)
     (T -3.1655645 0.5209217 2.0797653 2.5979273 0.60652924 1.5130169)
     (NIL -9.641175 -0.7795563 -1.3812904 1.1402545 2.6713824 2.8186238)
     (NIL -0.8194542 -1.285996 -3.3213663 0.5414703 1.3203964 1.7060363)
     (NIL 0.6583042 7.8061905 -4.023845 1.3807986 2.6542368 2.4800732)
     (T 5.0440073 1.8998103 7.443636 2.560404 1.0171034 1.0220116)
     (T 2.6787777 7.47538 -0.9560089 1.5907483 0.6731756 2.3729799)
     (NIL -7.144921 9.207903 -6.5612364 2.6914785 0.8190461 1.5237648)
     (T 3.644001 8.849237 -7.786231 1.465945 2.2756376 2.71814)
     (NIL -7.7720714 4.1524653 -9.596243 0.89156324 1.421622 0.9916198)
     (T 5.387995 7.285578 -9.008095 0.40077662 2.1044948 2.1095345)
     (NIL 7.4994125 3.0926652 2.9703732 0.45775062 1.7712276 0.37200028)
     (T 0.7389593 6.534685 -3.5038662 0.43935665 2.0924997 2.506431)
     (T 7.2512226 0.21281958 1.9044037 1.8553741 2.843639 1.4427772)
     (T 1.7044754 2.7443242 3.1843681 2.2643986 1.9260173 0.7760581))
    (:level
     (NIL -5.984037 9.006094 9.928047 1.5135772 0.3051138 1.5224588)
     (T -1.8634033 6.679529 -3.4411545 2.0783834 0.79687345 1.947331)
     (NIL -5.1794863 4.342486 -5.133106 1.5975327 1.8991017 1.4010589)
     (T 8.992794 1.4107656 -21.90385 0.34713683 2.9649026 2.7419193)
     (NIL -4.0269995 7.937664 -6.2961845 2.1244931 1.7503676 1.2527509)
     (T -2.8699994 -1.3551583 -13.155916 1.0505617 2.057347 2.4129183)
     (T 1.728342 -0.67538214 -17.782305 1.3630619 1.3649995 1.149587)
     (T 9.471319 2.6596308 -24.702744 1.2953086 1.0860236 2.5422938)
     (NIL 2.5127602 1.5654383 3.0464 2.142987 0.4056548 1.0998769)
     (NIL 6.0170918 -0.93649054 -3.0811424 1.9516413 0.8244061 0.910553)
     (T -6.242914 0.2994113 -16.477732 2.3538442 2.5398993 1.6846497)
     (NIL 3.8448 5.7359557 -6.4601307 0.5793477 1.8052423 2.9555492)
     (NIL -6.947844 3.796039 -6.8007793 1.8904755 0.93815964 2.8476381)
     (T -0.06263924 6.3740063 -1.5204391 2.1220615 1.4586632 1.6823351)
     (NIL 2.0345688 3.4934673 -18.315033 1.1854566 1.140461 2.3335404)
     (T 0.33486843 2.756864 -9.7147665 1.2257814 2.782247 2.5485768)
     (NIL 3.9551945 6.3112917 -9.659628 0.7628758 2.3238885 0.8560947)
     (T 5.5188007 0.8011546 -7.63398 1.2519884 2.9149227 2.8786833)
     (T -1.0321379 8.906797 0.77389336 1.2441475 2.456126 1.9693508)
     (T -1.7429466 5.54211 -3.0846386 2.9102042 2.4244115 2.5894134)
     (NIL 6.718809 3.7420874 2.0441513 1.3483918 1.1284153 0.31032577)
     (T -0.23350525 2.586779 -12.251811 1.7777128 1.3201034 1.9471884)
     (NIL 4.4447346 9.5613165 -11.013922 0.92367154 1.9159691 2.405618)
     (NIL 3.1876373 3.4041877 1.0416756 0.6603354 1.39221 1.7735968)
     (T -2.2423983 3.3080974 -3.7316246 1.077555 2.6284363 1.5969)
     (T -2.1649075 1.4789796 -4.9406643 1.9216847 2.8253477 1.6723645)
     (T 2.3890638 0.32658672 2.1099586 2.4753377 1.3062072 1.5303097)
     (NIL 2.6366282 2.9946952 4.3480988 2.3584445 1.5431798 1.1290189)
     (T 1.4455204 4.3760443 -10.521661 2.5300853 2.6970303 2.1546206)
     (NIL -2.84858 9.6060505 -24.013422 0.6822106 0.7013838 2.6963904)
     (T -8.69864 9.511311 -21.055 2.6171386 1.806586 2.3240855)
     (T 1.6509552 5.6947055 -2.6565628 2.5276222 2.2805042 0.88739026)
     (NIL 6.803196 -1.9891796 -17.208694 1.577054 1.2410247 0.59710884)
     (NIL -9.121494 7.366562 2.7278862 2.7310524 2.2801898 2.9581926)
     (T -8.439407 -0.19835234 -20.069908 1.8324718 2.2955394 0.93613225)
     (NIL 4.01412 8.640759 -16.92453 2.0963683 1.9675457 1.4141498)
     (NIL -7.372906 -1.1305923 -23.947134 2.1755948 0.57579815 2.5460489)
     (NIL -2.5202942 8.137219 -10.168243 0.8287516 0.9604823 0.7480578)
     (T -8.143706 2.7715569 -14.857231 2.4420753 2.9787388 1.7095416)
     (T 1.1058598 2.8398037 -15.420036 2.4396975 2.6921 2.6982412)
     (T -4.1893697 3.753643 -31.944485 1.71871 1.6853468 2.2052467)
     (NIL -2.9541636 -0.9509444 2.7439537 1.6232407 0.81870073 2.137443)
     (T -3.1249666 3.4435296 2.0369186 2.3797503 2.1594715 1.4761004)
     (NIL -9.225378 0.9641957 -41.011475 0.8451252 1.2206037 0.59678924)
     (T -0.1925087 3.2616158 7.1247787 1.8244851 0.49870375 1.3140943)
     (NIL -5.005324 -1.3171625 -33.37989 1.4792666 0.93925756 0.4705779)
     (T -1.4975309 -1.5179892 -25.66845 2.5919514 1.7516186 1.3136096)
     (NIL 3.2366867 -0.5371332 18.12011 1.0963367 1.6179748 2.8100848)
     (NIL -0.50050735 0.019636154 15.212288 1.1058366 1.9954853 2.7368011)
     (T -9.735603 7.353615 -32.653618 2.685259 2.940754 2.813208)
     (NIL -7.7244186 8.386778 16.397415 1.400882 1.5555322 1.1573498)
     (T -8.526783 8.607182 8.235245 1.9695113 2.272247 1.1365634)
     (T -0.7344799 0.023628712 -31.063738 2.137853 2.3338761 1.4891319)
     (T 5.9136295 0.2559166 6.448906 1.1774571 0.465093 0.48934776)
     (NIL 7.8438854 8.285818 4.695072 2.4945679 0.86262697 0.5161411)
     (T 7.0241127 4.255083 -41.16686 2.3501253 0.52827126 1.5548515)
     (T 5.299423 0.8125887 -33.102135 0.9011085 0.6049044 2.5197432)
     (NIL 5.238554 1.7628446 -24.753395 0.47343767 0.30042776 2.5879216)
     (T 2.0115948 7.0539913 -17.611656 2.180903 1.0848219 0.73195004)
     (T -3.778522 -1.2287254 -16.550789 0.7272955 1.8480184 0.91398436)
     (T 0.07428932 2.0576158 -23.85251 2.0439246 1.6357391 2.8236835)
     (NIL -7.7048683 -1.2521701 -33.086792 1.0468729 0.36073145 2.0032713)
     (T 4.7030973 8.289969 -31.891132 2.6331234 1.6397796 2.8471277)
     (NIL 0.9935074 6.757656 -33.957016 2.394887 2.9602544 0.89937395)
     (NIL -5.7913804 1.8122773 -53.775326 0.39674783 2.3977904 0.7802849)
     (T -7.5231767 1.683248 -35.610153 2.036768 2.0164778 2.0114582)
     (NIL -0.5055046 6.0450344 -52.45189 1.2070445 2.2900941 0.7342421)
     (T -3.4978461 1.8638229 -26.694302 1.5561535 1.3722997 1.792753)
     (NIL 0.85001755 7.21019 -35.822754 0.42670414 2.178045 2.2722242)
     (T -1.2825699 -1.9565711 -25.31216 1.4225814 2.842402 1.1537292)
     (T -5.778694 8.486599 -16.697517 2.756244 2.93788 1.3494711)
     (NIL 4.316864 1.552784 -45.004253 0.9412872 1.3856003 1.2681799)
     (NIL 4.538479 8.907503 -19.700436 2.0634224 1.6244423 2.5456147)
     (NIL 5.976837 6.3850517 -28.794928 0.83584356 0.4030258 2.1688902)
     (T -2.7311516 -0.25319386 -36.485313 1.5481532 2.2401757 0.8980917)
     (NIL -3.1834316 9.91835 -43.124813 2.4755318 1.6458869 1.1021863)
     (NIL -3.7248182 7.3964577 -41.482197 1.2209703 0.46458316 0.41615435)
     (T 8.000885 2.2295032 -16.603975 0.79520905 2.4141529 1.7930503)
     (NIL 6.0344677 9.390983 -39.90868 0.8660947 0.6693187 2.24972)
     (NIL -7.4630523 -1.1943889 -27.051682 1.3572857 0.98233855 1.9099474)
     (T 1.4661007 -1.5923557 -39.513615 1.3410246 2.3323927 2.3002746)
     (NIL -7.335546 9.009367 -32.65072 2.1134217 1.5615509 0.88607967)
     (NIL -6.1032915 9.493376 -23.77055 1.0549867 2.59157 1.4525397)
     (NIL 7.3706264 9.993706 -32.514645 2.8052652 0.89921784 2.1266134)
     (T -4.7863483 2.9973888 -30.983425 2.3690665 2.9898927 2.9848704)
     (T 4.7465134 3.3285217 -29.173067 2.3934176 1.6511142 0.64794385)
     (NIL -1.9778996 7.5330563 -38.760143 1.8105826 2.1812296 2.7121954)
     (NIL -4.743538 8.933394 -26.205341 2.258611 1.7173612 2.4653358)
     (NIL -6.5596604 6.1994133 -23.585667 2.4503934 0.7643097 0.8027404)
     (NIL 0.7552624 7.796299 -38.452724 2.042127 0.6532363 1.6161361)
     (T -6.017413 9.403614 -33.285477 1.2122529 2.5448816 2.898074)
     (NIL 5.5585594 6.4667463 -32.44291 0.44068113 0.5668967 2.9053276)
     (T 5.068283 3.4738436 -21.937134 0.880064 2.9036636 1.15549)
     (NIL 1.5294266 6.065366 -26.476448 1.4122248 2.0539327 1.9634621)
     (NIL -6.355076 3.1583347 -37.350906 1.8197176 1.2173474 0.72479177)
     (NIL -9.126499 5.5482426 -36.690372 2.3388596 1.1330667 0.3455333)
     (NIL 4.0947676 9.097332 -27.272928 1.1620796 1.1952507 0.30782166)
     (NIL -0.8513813 4.9338923 -31.776424 1.3957317 0.44664592 1.7636833)
     (NIL 6.818445 7.998412 -37.172497 1.7721858 2.6907809 1.3811104)
     (NIL -2.5381613 2.0511065 -31.73394 0.62981194 0.774751 2.0635889)
     (NIL 1.1388664 3.4550934 -23.190527 0.49243993 1.872714 0.43432558)
     (NIL 0.88396835 9.935525 -36.80077 0.30903026 1.03086 2.4228978)
     (NIL -1.944807 5.1484528 -25.554123 1.5734491 2.2613869 0.7378421)
     (NIL -1.6064339 9.358798 -37.871754 2.4740083 0.31086102 1.4842646)
     (NIL -2.439239 6.979903 -33.05802 2.707376 1.4279602 0.39317352)
     (T -6.67681 3.3725538 -33.30593 2.2534626 2.6743085 2.2802393)
     (T -7.8455544 7.13778 -29.451483 2.302569 1.3216388 1.8763337)
     (T -1.1974335 6.6897383 -31.480507 2.5801153 1.0874468 2.7674286)
     (NIL 4.272415 8.881327 -37.25167 1.9863548 1.6353567 2.4471848)
     (NIL 8.050556 6.731182 -21.965492 0.6286233 0.4055206 2.7154446)
     (T 0.43427277 7.5011606 -26.751888 1.801832 1.5454831 2.8886373)
     (T -0.324152 9.762457 -20.91737 1.8398802 1.2885079 2.1961336)
     (NIL 4.1317635 7.85927 -24.425877 1.1213193 1.9128821 0.81171334)
     (NIL 5.8586073 4.8423767 -34.596367 0.52341366 2.758076 0.6389252)
     (NIL -9.732302 7.409974 -27.889874 1.1920342 1.7389536 2.0034118)
     (NIL 2.911234 6.8455963 -20.68547 1.4057872 2.7215686 0.4519303)
     (T -1.6111736 3.0762596 -24.496508 1.2074156 2.7589984 0.67220604)
     (NIL 5.83375 9.479349 -35.66549 2.8118744 2.451313 0.97376245)
     (T 7.8079433 3.9876804 -32.422394 0.5756482 2.2961543 1.2507019)
     (NIL -17.807606 4.464242 -35.13764 2.765979 1.0446897 0.7090297)
     (NIL -24.085247 2.5872087 -33.209538 1.6660919 1.1828882 1.128126)
     (NIL -29.584728 7.022127 -29.71518 1.0032446 1.4870622 0.62345386)
     (NIL -1.4753819 7.801777 -23.723547 2.669889 1.547822 1.3664215)
     (NIL -13.2413435 6.0633917 -23.327206 2.240984 2.3043191 2.93558)
     (NIL -18.851254 4.5852966 -21.364853 1.7543967 2.678917 0.5335424)
     (NIL -19.131935 5.194169 -32.164288 0.762663 2.8168337 0.99251723)
     (NIL -13.433933 9.691134 -24.558784 0.37094584 2.8188884 0.8769744)
     (NIL -9.726227 3.1441593 -22.626896 0.585289 2.0100377 0.9551458)
     (T -5.0505877 3.1637726 -32.485943 2.9747555 2.2871177 1.8210926)
     (NIL -6.341448 4.972995 -25.242805 0.43574083 0.5227783 2.874567)
     (T -17.419266 2.6082764 -33.9804 2.9819734 1.6995625 2.9462879)
     (NIL -18.104925 7.9599276 -26.81964 0.93301016 1.7881377 1.153339)
     (NIL -1.7765274 8.333654 -27.12821 1.7785137 1.5522566 2.4888551)
     (NIL -13.361349 8.6116495 -33.166546 1.2767267 0.88426787 1.4286039)
     (NIL -0.69202614 8.494072 -22.368122 2.8031871 1.1761844 2.0116313)
     (NIL -17.55102 6.322708 -26.846165 1.0187354 1.4607518 1.6261182)
     (NIL -2.1225567 6.0633497 -35.2804 1.6071506 2.899908 2.151779)
     (NIL -4.872632 2.8117285 -34.78036 1.7255063 0.9169864 1.710289)
     (NIL -1.8416634 5.1677046 -34.378143 2.6245112 1.130672 1.5107679)
     (NIL -7.9273605 2.0463018 -33.257706 0.52911484 0.89887923 1.7327085)
     (T -3.7315712 2.0062046 -30.756672 1.7928174 2.0734386 2.4106073)
     (T -13.774204 6.588189 -30.004274 2.066455 2.7820919 2.2646754)
     (NIL -12.281661 5.4442253 -23.012932 0.5854255 1.7245562 1.2327647)
     (NIL -2.9133396 2.6526318 -22.167389 0.88376606 0.69961584 2.1785634)
     (T -5.37998 2.5537615 -32.12768 0.45194513 2.2488458 1.5992746)
     (T -13.0186825 3.2221804 -29.582039 2.3914633 2.4807518 2.861864)
     (T -11.115537 7.714183 -27.975168 2.5916119 2.3524528 0.80113816)
     (NIL -0.7195511 7.99553 -31.107807 1.1266941 1.1714153 2.6118793)))
  "A list of tests for AABB intersection. These were generated randomly
  and each result visually checked in a 3D rendering of the test
  scene.

  The format of each test case is as follows:

  (RESULT X0 Y0 Z0 X1 Y1 Z1)

  where X0 Y0 Z0, X1 Y1 Z1 are 2 points that construct a ray from P0,
  and where RESULT is T or NIL indicating whether or not the AABB
  intersected with the BSP.")


(defun vec3-cmp (x0 y0 z0 x1 y1 z1 eps)
  (and (< (abs (- x1 x0)) eps) (< (abs (- y1 y0)) eps) (< (abs (- z1 z0)) eps)))

(defconstant +test-eps+ 0.0001)

(defun remove-nth (n list)
  (loop for x in list
        for ii from 0
        unless (= ii n) collect x))

(define-test bsp-ray-query :parent bsp
  (loop for model in '(:funny-house :level)
        for bsp = (build-bsp-from-meshes (list (get-model model)))
        for ray-test-cases = (cdr (assoc model *ray-tests*))
        do (loop
            for test-case in ray-test-cases
            for recorded-result = (if (listp (car test-case)) (car test-case) nil)
            for (x0 y0 z0 x1 y1 z1) = (if (listp (car test-case)) (cdr test-case) test-case)
            for p0 = (vec3 x0 y0 z0)
            for p1 = (vec3 x1 y1 z1)
            for d = (vunit (v- p1 p0)) do
              (let ((found-result nil))
                 (bsp:do-intersecting (result bsp p0 d)
                   (setf found-result t)
                   (true recorded-result)
                   (when recorded-result
                     (destructuring-bind (rx ry rz) recorded-result
                       (true (vec3-cmp rx ry rz (bsp:ray-result-x result) (bsp:ray-result-y result) (bsp:ray-result-z result) +test-eps+)))))
                (when (not found-result)
                  (false recorded-result))))))

(define-test bsp-aabb-query :parent bsp
  (loop for model in '(:funny-house :level)
        for bsp = (build-bsp-from-meshes (list (get-model model)))
        for test-cases = (cdr (assoc model *aabb-tests*))
        do (loop
            for test-case in test-cases
            for recorded-result = (car test-case)
            for (x y z hx hy hz) = (cdr test-case) do
              (let ((found-result nil))
                (bsp:do-overlapping (result bsp (space:region (- x hx) (- y hy) (- z hz) (* 2.0 hx) (* 2.0 hy) (* 2.0 hz)))
                  (declare (ignore result))
                  (setf found-result t))
                (is #'eq recorded-result found-result)))))

(defun %check-bsp-equal-mesh (mesh0 mesh1)
  "Helper for CHECK-BSP-EQUAL, mesh equality"
  (is #'= (length (space:mesh-vertices mesh0)) (length (space:mesh-vertices mesh1)))
  (is #'= (length (space:mesh-faces mesh0)) (length (space:mesh-faces mesh1)))
  (loop for ii below (length (space:mesh-vertices mesh0)) do
    (is #'= (aref (space:mesh-vertices mesh0) ii) (aref (space:mesh-vertices mesh1) ii)))
  (loop for ii below (length (space:mesh-faces mesh0)) do
    (is #'= (aref (space:mesh-faces mesh0) ii) (aref (space:mesh-faces mesh1) ii))))

(defun %check-bsp-equal-node (bsp-node0 bsp-node1)
  "Helper for CHECK-BSP-EQUAL, recursive bsp node comparison"
  (is #'eql (bsp::bsp-node-leaf-p bsp-node0) (bsp::bsp-node-leaf-p bsp-node1))
  (cond ((bsp::bsp-node-leaf-p bsp-node0)
         (is #'eql (bsp::bsp-node-solid-p bsp-node0) (bsp::bsp-node-solid-p bsp-node1))
         (when (bsp::bsp-node-solid-p bsp-node0)
           (true (and (bsp::bsp-node-tri-mesh bsp-node0) (bsp::bsp-node-tri-mesh bsp-node1)))
           (%check-bsp-equal-mesh (bsp::bsp-node-tri-mesh bsp-node0) (bsp::bsp-node-tri-mesh bsp-node1))))
        (t
         (is #'= (bsp::bsp-node-px bsp-node0) (bsp::bsp-node-px bsp-node1))
         (is #'= (bsp::bsp-node-py bsp-node0) (bsp::bsp-node-py bsp-node1))
         (is #'= (bsp::bsp-node-pz bsp-node0) (bsp::bsp-node-pz bsp-node1))
         (is #'= (bsp::bsp-node-pd bsp-node0) (bsp::bsp-node-pd bsp-node1))
         (%check-bsp-equal-node (bsp::bsp-node-front bsp-node0) (bsp::bsp-node-front bsp-node1))
         (%check-bsp-equal-node (bsp::bsp-node-behind bsp-node0) (bsp::bsp-node-behind bsp-node1)))))

(defun check-bsp-equal (bsp0 bsp1)
  "Compare two BSP, asserting if they are not equal. This is for
testing serialization, and nothing else. Because this is for testing
ser, exact FP comparisons are used."
  (is #'= (bsp::bsp-eps bsp0) (bsp::bsp-eps bsp1))
  (%check-bsp-equal-node (bsp::bsp-root bsp0) (bsp::bsp-root bsp1)))

(define-test bsp-serialize :parent bsp
  "Ser + deser a bsp, return the new bsp"
  (loop for model in '(:funny-house :level)
        for bsp = (build-bsp-from-meshes (list (get-model model))) do
          (with-open-file (stream #P"test.bsp" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (space:serialize bsp stream #'identity))
          (with-open-file (stream #P"test.bsp" :direction :input :element-type '(unsigned-byte 8))
            (let ((new-bsp (bsp:make-bsp)))
              (space:deserialize new-bsp stream #'identity)
              (check-bsp-equal new-bsp bsp)))))
