unit delaunay3;

interface

{.$DEFINE PERFORMANCELOG}

uses
  Logger,
  MyThreads, MyStr,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Math,
  System.SysUtils,
  WinApi.Windows,
  System.Types,
  System.Classes;

type
  TDLValue = Double;
  TDLStoredCoordinate = Double;
  TDLCoordinate = Double;

  TDLID = RawByteString;

const
  soXY = -1;
  soUnsorted = 0;
  soYX = 1;

  poeEdge12 = 12;
  poeEdge23 = 23;
  poeEdge31 = 31;
  poeNoEdge = -1;

  sideLeft = 1;
  sideColineair = 0;
  sideRight = -1;

  ccTab = #$09;

  dotFormat: TFormatSettings = (DecimalSeparator:'.');

type
  TDLPoint = class; // forward
  TDLTriangle = class; // forward

  TDLExtent = record
    xMin: TDLCoordinate;
    yMin: TDLCoordinate;
    xMax: TDLCoordinate;
    yMax: TDLCoordinate;

    function IsNoWorld: Boolean;
    class function NoWorld: TDLExtent; static;

    procedure ExtendExtent(p: TDLPoint); overload;
    procedure ExtendExtent(t: TDLTriangle); overload;
    function ExtentUnion(const aExtent: TDLExtent): TDLExtent;
    function ExtentIntersection(const aExtent: TDLExtent): TDLExtent;
    function ExtentRound: TDLExtent;
    function SameExtent(const aExtent: TDLExtent): Boolean;

    function ToString: string;
  end;

  TDLPoint = class
  constructor Create(ax, ay: TDLCoordinate; aValue: TDLValue=NaN);
  public
    x: TDLCoordinate;
    y: TDLCoordinate;
    value: TDLValue;
//    ID: Integer;
    function IsSame(ax, ay: TDLCoordinate): Boolean;
    function contains(aValue: TDLValue): Boolean;
    function ToString: string; override;
  end;

  // never owns points always refs
  TDLPointList = class(TList<TDLPoint>)
  constructor Create;
  private
    FSortOrder: Integer; // soXY = -1; soUnsorted = 0; soYX = 1;
  public
    property SortOrder: Integer read FSortOrder;

    procedure QuickDelete(aIndex: Integer);
    procedure SortXY(aThreadPool: TMyThreadPool=nil; aParam: Integer=0);
    procedure SortYX(aThreadPool: TMyThreadPool=nil; aParam: Integer=0);
    procedure Resort(aThreadPool: TMyThreadPool=nil; aParam: Integer=0);
//    function RemoveDuplicatesXY: Boolean;
    procedure GetMinMax(expandborderwith: Integer; out minx, miny, maxx, maxy: TDLCoordinate);
//    function IndexOf(ax, ay: TDLCoordinate): Integer; overload;
    procedure ExtendExtent(var aExtent: TDLExtent);
  end;

  TDLTriangle = class
  private
    // points, ordered clockwise
    FVertex1: TDLPoint;
    FVertex2: TDLPoint;
    FVertex3: TDLPoint;
    // neighbouring triangles
    FNB1: TDLTriangle; // side oposite to Vertex1
    FNB2: TDLTriangle; // side oposite to Vertex2
    FNB3: TDLTriangle; // side oposite to Vertex3
    // normal vector
    FNx: TDLCoordinate;
    FNy: TDLCoordinate;
    FNz: TDLCoordinate;
    FNCalculated: Boolean;
    FDisabled: Boolean;
//    procedure Assign(aTriangle: TDLTriangle);
//    function GetPOE(aTriangle: TDLTriangle): Integer;
//    function GetNB(aVertexA, aVertexB: TDLPoint): TDLTriangle;
  public
    property Vertex1: TDLPoint read FVertex1;
    property Vertex2: TDLPoint read FVertex2;
    property Vertex3: TDLPoint read FVertex3;
    property NB1: TDLTriangle read FNB1;
    property NB2: TDLTriangle read FNB2;
    property NB3: TDLTriangle read FNB3;
  private
    procedure ShiftNil;
//    function ContainsValue(aValue: TDLValue): Boolean;
//    function ContainsPoint(aPoint: TDLPoint): Boolean; overload;
    function ContainsPoint(x, y: TDLCoordinate): Boolean; overload;
//    function ContainsVertex(aVertex: TDLPoint): Boolean;
//    function PointOnEdge(aPoint: TDLPoint): Integer;
    function IsGhost: Boolean;
//    function IsOnHull: Boolean;
//    function IsUnlinked: Boolean; // not linked to any other triangle
    procedure Unlink;
    procedure UnlinkTo(aTriangle: TDLTriangle);
//    procedure Relink(aSrcTriangle, aDstTriangle: TDLTriangle);
    function LinkTo(aTriangle: TDLTriangle; aVertexA, aVertexB: TDLPoint): Integer;
//    function ContainsEdge(p1, p2: TDLPoint): Boolean;
//    function ContainsEdgeInOrder(p1, p2: TDLPoint): Boolean;
//    function ValueOnPoint(aPoint: TDLPoint): TDLValue; overload;
    function ValueOnPoint(x, y: TDLCoordinate): TDLValue; overload;
    property NCalculated: Boolean read FNCalculated write FNCalculated;
    property Disabled: Boolean read FDisabled write FDisabled;
    function NextLeftTriangle(aCenter: TDLPoint): TDLTriangle;
    function NextRightTriangle(aCenter: TDLPoint): TDLTriangle;
//    function LastLeftTriangle(aCenter: TDLPoint): TDLTriangle;
//    function LastRightTriangle(aCenter: TDLPoint): TDLTriangle;
//    function NextHullEdge(var poe: Integer): TDLTriangle;
//    function PrevHullEdge(var poe: Integer): TDLTriangle;
//    function poeToVertexCW(poe: Integer): TDLPoint;
//    function poeToVertexCCW(poe: Integer): TDLPoint;
//    function LengthOfLongestEdge: TDLCoordinate;
    function CheckEdgeLengths(aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate): Boolean;
    function HasNoData(aNoData: TDLValue=NaN): Boolean;
  end;

  TDLTriangleList = class(TList)
  constructor Create(aOwnsTriangles: Boolean);
  private
    FOwnsTriangles: Boolean;
    function GetTriangle(aIndex: Integer): TDLTriangle;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  private
    procedure QuickDelete(aIndex: Integer);
//    function QuickRemove(aItem: Pointer): Integer; inline;
    property Triangles[aIndex: Integer]: TDLTriangle read GetTriangle; default;
//    function PointToTriangle(aPoint: TDLPoint): TDLTriangle;
//    function PointToValue(aPoint: TDLPoint; aNoValue: TDLValue): TDLValue; overload;
//    procedure MarkAsUncalculated(aPoint: TDLPoint; aAffectedTriangles: TList = nil);
//    procedure FindTriangles(aVertex: TDLPoint; aTriangles: TList);
//    function FindFirstTriangle(aVertex: TDLPoint): TDLTriangle;
//    function FindFirstHullTriangle: TDLTriangle;
//    function ContainsVertex(aVertex: TDLPoint): Boolean;
//    function ContainsGhostTriangle: Boolean;
    procedure RemoveGhostTriangles;
//    function CheckConsistency: Boolean;
//    function CheckUnlinked(aExceptions: TDLTriangleList = nil): Boolean;
    function DisableTooLongEdgesAndNoData(aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; aNodata: TDLValue=NaN): Boolean;
  public
    function PointToValue(x, y: TDLCoordinate; aNoValue: TDLValue): TDLValue; // overload;
  end;

  TDLNode = class
  constructor Create(aPoints: TDLPointList);
  destructor Destroy; override;
  private
    FPoints: TDLPointList;
    FTriangles: TDLTriangleList; //refs
    FChild1: TDLNode;
    FChild2: TDLNode;
  public
    property Points: TDLPointList read FPoints;
    property Triangles: TDLTriangleList read FTriangles;
    property Child1: TDLNode read FChild1;
    property Child2: TDLNode read FChild2;
    procedure Clear;
    // triangulation steps
    procedure DevidePoints(aThreadPool: TMyThreadPool=nil; aParam: Integer=0);
    procedure Triangulate(aThreadPool: TMyThreadPool=nil; aParam: Integer=0);
    procedure Merge(aThreadPool: TMyThreadPool=nil; aParam: Integer=0);
    procedure RemoveGhostsTriangles;
  end;

  TDLNet = class
  constructor Create;
  destructor Destroy; override;
  private
    FPoints: TObjectDictionary<TDLID, TDLPoint>;
    FTriangles: TDLTriangleList; // owned triangles
//    FOrphanes: Integer;
    function RemoveDuplicates: Boolean;
  public
    property Points: TObjectDictionary<TDLID, TDLPoint> read FPoints;
    property Triangles: TDLTriangleList read FTriangles;
//    procedure setPoints(aPoints: TDLPointList);
    // action
    procedure Clear;
    procedure Triangulate(
      aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; aNodata: TDLValue=NaN;
      aThreadPool: TMyThreadPool=nil; aCheckForDuplicates: Boolean=True; aParam: Integer=0);
    // inquire
//    function PointToTriangle(aPoint: TDLPoint): TDLTriangle;
//    function PointToValue(aPoint: TDLPoint; aNoValue: TDLValue): TDLValue;
    function PointsToExtent: TDLExtent;
    // diff
//    procedure AddPoint(aPoint: TDLPoint; aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; aNoData: TDLValue=NaN); overload;
//    function RemovePoint(aID: Integer): Boolean; overload;
//    function ChangePoint(aID: Integer; aValue: TDLValue): Boolean; overload;

//    procedure AddPoint(aPoint: TDLPoint; aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; var aExtent: TDLExtent; aNoData: TDLValue=NaN); overload;
//    function RemovePoint(aID: Integer; var aExtent: TDLExtent): Boolean; overload;
//    function ChangePoint(aID: Integer; aValue: TDLValue; var aExtent: TDLExtent): Boolean; overload;

    procedure SaveToFile(const aFileName: string);
    function ValueAtPoint(var aCursor: TDLTriangle; x, y: TDLCoordinate; aNoValue: TDLValue): TDLValue;
  end;



implementation

//function Distance(p, q: TDLPoint): TDLCoordinate;
//function Side(p, q, r: TDLPoint): Integer; inline; overload;
//function Side(p, q: TDLPoint; rx, ry: TDLCoordinate): Integer; inline; overload;
//function EdgeContainsValue(p, q: TDLPoint; aValue: TDLValue): Boolean;
//function CompareXY(p1, p2: TDLPoint): Integer; inline;
//function CompareYX(p1, p2: TDLPoint): Integer; inline;

function InCircle(p1, p2, p3, p4: TDLPoint): Boolean; inline;
begin
  Result := ((p4.y - p1.y) * (p2.x - p3.x) + (p4.x - p1.x) * (p2.y - p3.y)) *
    ((p4.x - p3.x) * (p2.x - p1.x) - (p4.y - p3.y) * (p2.y - p1.y)) >
    ((p4.y - p3.y) * (p2.x - p1.x) + (p4.x - p3.x) * (p2.y - p1.y)) *
    ((p4.x - p1.x) * (p2.x - p3.x) - (p4.y - p1.y) * (p2.y - p3.y));
end;

function Distance(p, q: TDLPoint): TDLCoordinate; inline;
begin
  Result := sqrt(sqr(p.x - q.x) + sqr(p.y - q.y));
end;

function Side(p, q, r: TDLPoint): Integer; inline; overload;
var
  anarea: TDLCoordinate;
begin
  anarea := ((r.y - p.y) * (q.x - p.x)) - ((r.x - p.x) * (q.y - p.y));
  if anarea > 0
  then Result := sideLeft // 1
  else if anarea = 0
  then Result := sideColineair // 0
  else Result := sideRight; // -1
end;

function Side(p, q: TDLPoint; rx, ry: TDLCoordinate): Integer; inline; overload;
var
  anarea: TDLCoordinate;
begin
  anarea := ((ry - p.y) * (q.x - p.x)) - ((rx - p.x) * (q.y - p.y));
  if anarea > 0
  then Result := sideLeft // 1
  else if anarea = 0
  then Result := sideColineair // 0
  else Result := sideRight; // -1
end;

procedure Flip(T1, T2: TDLTriangle);
begin
  if T2.FVertex1 = T1.FVertex3 then
  begin
    T2.FVertex1 := nil;
    T1.FNB2 := T2.FNB3;
  end
  else
  begin
    if T2.FVertex2 = T1.FVertex3 then
    begin
      T2.FVertex2 := nil;
      T1.FNB2 := T2.FNB1;
    end
    else
    begin
      T2.FVertex3 := nil;
      T1.FNB2 := T2.FNB2;
    end;
  end;
  // let the triangle which points to t1 now point to t2
  // always same position in t3
  T2.ShiftNil; // put the nil in the middle position
  T1.FNB3.FNB1 := T2; // ok
  T2.FNB1 := T1; // ok
  T2.FNB3 := T1.FNB3; // ok
  T1.FNB3 := T2; // ok
  T1.FVertex1 := T2.FVertex3; // ok
  // update the triangle which previously referenced to t2 to
  // point to t1 at that position
  if T1.FNB2.FNB1 = T2
  then T1.FNB2.FNB1 := T1;
  if T1.FNB2.FNB2 = T2
  then T1.FNB2.FNB2 := T1;
  if T1.FNB2.FNB3 = T2
  then T1.FNB2.FNB3 := T1;
  // the swap is now complete,we have successfully removed one edge
  // and connectivity is maintained
end;

function CountRight(T1, T2: TDLTriangle): Integer;
var
  i: Integer;
  tcurrent: TDLTriangle;
begin
  i := 0;
  tcurrent := T2;
  if Side(T1.FVertex1, T1.FVertex3, tcurrent.FVertex1) = sideLeft
  then inc(i);
  tcurrent := tcurrent.FNB3;
  while (tcurrent.FNB3 <> T2) and (i < 1) do
  begin
    if Side(T1.FVertex1, T1.FVertex3, tcurrent.FVertex1) = sideLeft
    then inc(i);
    tcurrent := tcurrent.FNB3;
  end;
  if Side(T1.FVertex1, T1.FVertex3, tcurrent.FVertex1) = sideLeft
  then inc(i);
  Result := i;
end;

function CountLeft(T1, T2: TDLTriangle): Integer;
var
  i: Integer;
  lcurrent: TDLTriangle;
begin
  i := 0;
  lcurrent := T2;
  if Side(T1.FVertex1, T1.FVertex3, lcurrent.FVertex3) = sideLeft
  then inc(i);
  lcurrent := lcurrent.FNB1;
  while (lcurrent.FNB1 <> T2) and (i < 1) do
  begin
    if Side(T1.FVertex1, T1.FVertex3, lcurrent.FVertex3) = sideLeft
    then inc(i);
    lcurrent := lcurrent.FNB1;
  end;
  if Side(T1.FVertex1, T1.FVertex3, lcurrent.FVertex3) = sideLeft
  then inc(i);
  Result := i;
end;

function GetRightCandidate(T1: TDLTriangle): TDLTriangle;
var
  FPC, NPC: TDLTriangle;
  NPCpoint: TDLPoint;
begin
  FPC := T1.FNB1;
  if ((FPC.FVertex3.x = T1.FVertex3.x) and (FPC.FVertex3.y = T1.FVertex3.y)) or
    ((FPC.FVertex3.x = T1.FVertex1.x) and (FPC.FVertex3.y = T1.FVertex1.y))
  then Exception.Create('equal point problem GETright POSITION 1');
  if Side(T1.FVertex1, T1.FVertex3, FPC.FVertex3) <> sideLeft
  then Result := nil
  else
  begin
    if not Assigned(FPC.FNB2.FVertex2)
    then Result := FPC // the neighbour is a ghost: only one candidate possible
    else
    begin
      // the neighbour is not a ghost
      NPC := FPC.FNB2;
      if FPC.FVertex3 = NPC.FVertex1
      then NPCpoint := NPC.FVertex2
      else
      begin
        if FPC.FVertex3 = NPC.FVertex2
        then NPCpoint := NPC.FVertex3
        else NPCpoint := NPC.FVertex1;
      end;
      if ((NPCpoint.x = T1.FVertex3.x) and (NPCpoint.y = T1.FVertex3.y)) or
        ((NPCpoint.x = T1.FVertex1.x) and (NPCpoint.y = T1.FVertex1.y))
      then Exception.Create('equal point problem GETRIGHT POSITION 2');
      if Side(T1.FVertex1, T1.FVertex3, NPCpoint) <> sideLeft
      then Result := FPC // BBBBBB
      else
      begin
        if InCircle(FPC.FVertex3, T1.FVertex1, T1.FVertex3, NPCpoint) then
        begin
          // the npcpoint is within the circle !!
          Flip(FPC, NPC);
          Result := GetRightCandidate(T1);
        end
        else Result := FPC;
      end;
    end;
  end;
end;

function GetLeftCandidate(T1: TDLTriangle): TDLTriangle;
var
  FPC, NPC: TDLTriangle;
  NPCpoint: TDLPoint;
begin
  { the first PC is vertex3 from ttriangle(t1).nb1
    if vertex3 from neighbour1 is not to the right of V3 -> V1
    the angle is > 180 and there are no other candidates possible: nil }
  FPC := T1.FNB3;
  if ((FPC.FVertex1.x = T1.FVertex3.x) and (FPC.FVertex1.y = T1.FVertex3.y)) or
     ((FPC.FVertex1.x = T1.FVertex1.x) and (FPC.FVertex1.y = T1.FVertex1.y))
  then Exception.Create('equal point problem GETLEFT POSITION 1');
  if Side(T1.FVertex1, T1.FVertex3, FPC.FVertex1) <> sideLeft
  then Result := nil
  else
  begin
    if not Assigned(FPC.FNB2.FVertex2)
    then Result := FPC // the neighbour is a ghost: only one candidate possible
    else
    begin
      // the neighbour is not a ghost
      NPC := FPC.FNB2;
      if FPC.FVertex1 = NPC.FVertex3
      then NPCpoint := NPC.FVertex2
      else
      begin
        if FPC.FVertex1 = NPC.FVertex2 then
          NPCpoint := NPC.FVertex1
        else NPCpoint := NPC.FVertex3;
      end;
      if Side(T1.FVertex1, T1.FVertex3, NPCpoint) <> sideLeft
      then Result := FPC // BBBBBBBBBB
      else
      begin
        if InCircle(T1.FVertex1, T1.FVertex3, FPC.FVertex1, NPCpoint) then
        begin
          Flip(FPC, NPC);
          Result := GetLeftCandidate(T1); // RECURSION !!
        end
        else Result := FPC;
      end;
    end;
  end;
end;

procedure Merge(T1: TDLTriangleList);
{ triangle T1.items[1] is the base triangle for the merge.
  this triangle already points to the LL and RR sets so
  no more info needs to be passed to this procedure }
{ from the left triangle we can derive LLcandidates, from the
  right triangle we can derive RR candidates }
var
  finished: Boolean;
  RC, LC, BT: TDLTriangle;
  rcfound, lcfound: Boolean;
begin
  BT := TDLTriangleList(T1)[1]; // there can only be 2 !
  finished := False;
  while not finished do
  begin
    RC := GetRightCandidate(BT);
    LC := GetLeftCandidate(BT);
    if (not Assigned(RC)) and (not Assigned(LC))
    then finished := True // no candidates submitted so we finished
    else
    begin
      lcfound := Assigned(LC);
      rcfound := Assigned(RC);
      if lcfound and rcfound then
      begin
        if not InCircle(RC.FVertex3, BT.FVertex1, BT.FVertex3, LC.FVertex1)
        then lcfound := False;
      end;
      if lcfound then
      begin
        LC.FVertex2 := BT.FVertex3;
        BT.FVertex1 := LC.FVertex1;
        BT.FNB3 := LC.FNB3;
        BT.FNB3.FNB1 := BT;
        LC.FNB1 := BT.FNB2;
        if LC.FNB1.FNB1 = BT
        then LC.FNB1.FNB1 := LC;
        if LC.FNB1.FNB2 = BT
        then LC.FNB1.FNB2 := LC;
        if LC.FNB1.FNB3 = BT
        then LC.FNB1.FNB3 := LC;
        BT.FNB2 := LC;
        LC.FNB3 := BT;
      end
      else
      begin
        RC.FVertex2 := BT.FVertex1;
        BT.FVertex3 := RC.FVertex3;
        BT.FNB1 := RC.FNB1;
        BT.FNB1.FNB3 := BT;
        RC.FNB3 := BT.FNB2;
        if RC.FNB3.FNB1 = BT
        then RC.FNB3.FNB1 := RC;
        if RC.FNB3.FNB2 = BT
        then RC.FNB3.FNB2 := RC;
        if RC.FNB3.FNB3 = BT
        then RC.FNB3.FNB3 := RC;
        BT.FNB2 := RC;
        RC.FNB1 := BT;
      end;
    end;
  end;
end;

procedure MergeLR(ParentTriangleList, Child1TriangleList, Child2TriangleList: TDLTriangleList);
var
  i, il, ir: Integer;
  LLstart, RRstart: TDLTriangle;
  LLcurrent, RRcurrent: TDLTriangle;
  RC, rcprevious, LC, lcprevious, cycles, maxcycles: Integer;
  foundleft, foundright: Boolean;
  dist, tempdist: TDLCoordinate;
  LLRRTriangle: TDLTriangle;
  RRLLTriangle: TDLTriangle;
begin
  // Child1TriangleList is always left, Child2TriangleList always right
  il := 0;
  while Assigned(Child1TriangleList[il].FVertex2)
  do inc(il); // there's always a triangle on the convex hull. find the first one in the list
  LLstart := Child1TriangleList[il];
  ir := 0;
  while Assigned(Child2TriangleList[ir].FVertex2)
  do inc(ir); // there's always a triangle on the convex hull. find the first one in the list
  RRstart := Child2TriangleList[ir];
  // there will always be a base triangle nomatter
  // what configuration so we can safely Create it here
  // add triangle LL -> RR
  LLRRTriangle := TDLTriangle.Create;
  ParentTriangleList.add(LLRRTriangle);
  // add triangle RR -> LL
  RRLLTriangle := TDLTriangle.Create;
  ParentTriangleList.add(RRLLTriangle);
  // try to find a ll start triangle
  LLcurrent := LLstart;
  rcprevious := CountRight(LLcurrent.FNB1, RRstart);
  foundleft := False;
  cycles := 0;
  maxcycles := Child1TriangleList.Count;
  while (not foundleft) and (cycles <= maxcycles + 1) do
  begin
    RC := CountRight(LLcurrent, RRstart);
    if (RC <> 0) and (rcprevious = 0)
    then foundleft := True // i.e. LLcurrent
    else
    begin
      rcprevious := RC;
      LLcurrent := LLcurrent.FNB3; // move up
      inc(cycles);
    end;
  end;
  // try to find a RR start triangle
  RRcurrent := RRstart;
  lcprevious := CountLeft(RRcurrent.FNB3, LLstart);
  foundright := False;
  cycles := 0;
  maxcycles := Child2TriangleList.Count;
  while ((foundright = False) and (cycles <= maxcycles + 1)) do
  begin
    LC := CountLeft(RRcurrent, LLstart);
    if (LC <> 0) and (lcprevious = 0)
    then foundright := True // i.e. LLcurrent
    else
    begin
      lcprevious := LC;
      RRcurrent := RRcurrent.FNB1; // move up OOOOO
      inc(cycles);
    end;
  end;
  if (foundleft = False) and (foundright = False) then
  begin
    dist := Distance(Child1TriangleList[0].FVertex3, RRstart.FVertex3);
    LLcurrent := Child1TriangleList[0];
    for i := 0 to Child1TriangleList.Count - 1 do
    begin
      tempdist := Distance(Child1TriangleList[i].FVertex3, RRstart.FVertex3);
      if tempdist < dist then
      begin
        dist := tempdist;
        LLcurrent := Child1TriangleList[i];
      end;
    end;
    dist := Distance(Child2TriangleList[0].FVertex1, LLstart.FVertex3);
    RRcurrent := Child2TriangleList[0];
    for i := 0 to Child2TriangleList.Count - 1 do
    begin
      tempdist := Distance(Child2TriangleList[i].FVertex1, LLstart.FVertex3);
      if tempdist < dist then
      begin
        dist := tempdist;
        RRcurrent := Child2TriangleList[i];
      end;
    end;
    LLRRTriangle.FVertex3 := RRcurrent.FVertex1;
    LLRRTriangle.FVertex2 := nil;
    LLRRTriangle.FVertex1 := LLcurrent.FVertex3;
    LLRRTriangle.FNB1 := RRcurrent;
    LLRRTriangle.FNB2 := RRLLTriangle;
    LLRRTriangle.FNB3 := LLcurrent;
    LLcurrent.FNB1 := LLRRTriangle;
    RRcurrent.FNB3 := LLRRTriangle;
    RRLLTriangle.FVertex1 := RRcurrent.FVertex1;
    RRLLTriangle.FVertex2 := nil;
    RRLLTriangle.FVertex3 := LLcurrent.FVertex3;
    RRLLTriangle.FNB1 := LLcurrent.FNB2;
    RRLLTriangle.FNB2 := LLRRTriangle;
    RRLLTriangle.FNB3 := RRcurrent.FNB2;
    RRcurrent.FNB2.FNB1 := RRLLTriangle;
    LLcurrent.FNB2.FNB3 := RRLLTriangle;
  end;
  if foundleft and (not foundright) then
  begin
    // try to find a RR start triangle
    RRcurrent := RRstart;
    foundright := False;
    cycles := 0;
    maxcycles := Child2TriangleList.Count;
    while (not foundright) and (cycles <= maxcycles + 10) do
    begin
      // lc := countleft(RRcurrent,LLstart);
      if (Side(RRcurrent.FVertex1, RRcurrent.FVertex3, LLcurrent.FVertex3) = sideLeft) and
        (Side(RRcurrent.FNB3.FVertex1, RRcurrent.FNB3.FVertex3, LLcurrent.FVertex3) <> sideLeft)
      then foundright := True // i.e. LLcurrent
      else
      begin
        RRcurrent := RRcurrent.FNB1; // move up OOOOO
        inc(cycles);
      end;
    end;
    // NEW
  end
  else
  begin
    if foundright and not foundleft then
    begin
      LLcurrent := LLstart;
      cycles := 0;
      maxcycles := TDLTriangleList(Child1TriangleList).Count;
      while (not foundleft) and (cycles <= maxcycles + 10) do
      begin
        if (Side(LLcurrent.FVertex1, LLcurrent.FVertex3, RRcurrent.FVertex1) = sideLeft) and
          (Side(LLcurrent.FNB1.FVertex1, LLcurrent.FNB1.FVertex3, RRcurrent.FVertex1) <> sideLeft)
        then foundleft := True // i.e. LLcurrent
        else
        begin
          LLcurrent := LLcurrent.FNB3; // move up
          inc(cycles);
        end;
      end;
      // NEW
    end;
  end;
  if foundleft or foundright then
  begin
    LLRRTriangle.FVertex1 := RRcurrent.FVertex1;
    LLRRTriangle.FVertex2 := nil;
    LLRRTriangle.FVertex3 := LLcurrent.FVertex3;
    LLRRTriangle.FNB1 := LLcurrent.FNB1;
    LLRRTriangle.FNB2 := RRLLTriangle;
    LLRRTriangle.FNB3 := RRcurrent.FNB3;
    // assign the back neighbours !!
    LLRRTriangle.FNB1.FNB3 := LLRRTriangle;
    LLRRTriangle.FNB3.FNB1 := LLRRTriangle;
    RRLLTriangle.FVertex1 := LLcurrent.FVertex3;
    RRLLTriangle.FVertex2 := nil;
    RRLLTriangle.FVertex3 := RRcurrent.FVertex1;
    RRLLTriangle.FNB1 := RRcurrent;
    RRLLTriangle.FNB2 := LLRRTriangle;
    RRLLTriangle.FNB3 := LLcurrent;
    // assign the back neighbours !!
    RRcurrent.FNB3 := RRLLTriangle;
    LLcurrent.FNB1 := RRLLTriangle;
    // now call the actual merge procedure
    Merge(ParentTriangleList); // : triangle[1] is the base triangle
  end;
end;

function IsEqualXY(p1, p2: TDLPoint): Boolean; inline;
begin
  if p1.x = p2.x
  then Result := p1.y = p2.y
  else Result := False;
end;

function CompareX(p1, p2: Pointer): Integer; inline;
// compare two coordinates based on x
begin
  if TDLPoint(p1).x > TDLPoint(p2).x
  then Result := 1
  else
  begin
    if TDLPoint(p1).x < TDLPoint(p2).x
    then Result := -1
    else Result := 0;
  end;
end;

function CompareY(p1, p2: Pointer): Integer; inline;
// compare two coordinates based on y
begin
  if TDLPoint(p1).y > TDLPoint(p2).y
  then Result := 1
  else
  begin
    if TDLPoint(p1).y < TDLPoint(p2).y
    then Result := -1
    else Result := 0; // thus they are equal in Y
  end;
end;

function CompareXY(p1, p2: TDLPoint): Integer; inline;
// compare two coordinates based on x then y
begin
  if TDLPoint(p1).x > TDLPoint(p2).x
  then Result := 1
  else
  begin
    if TDLPoint(p1).x < TDLPoint(p2).x
    then Result := -1
    else Result := CompareY(p1, p2); // equal in x-direction,  compare y coordinates
  end;
end;

function CompareYX(p1, p2: TDLPoint): Integer; inline;
// compare two coordinates based on y then x
begin
  if TDLPoint(p1).y > TDLPoint(p2).y
  then Result := 1
  else
  begin
    if TDLPoint(p1).y < TDLPoint(p2).y
    then Result := -1
    else Result := CompareX(p1, p2); // equal in y-direction, compare x coordinates
  end;
end;

function EdgeContainsValue(p, q: TDLPoint; aValue: TDLValue): Boolean; inline;
begin
  if Assigned(p) and Assigned(q)
  then Result := (p.Value <= aValue) and (aValue < q.Value) or (q.Value <= aValue) and (aValue < p.Value)
  else
  begin
    if Assigned(p)
    then Result := p.contains(aValue)
    else
    begin
      if Assigned(q)
      then Result := q.contains(aValue)
      else Result := False;
    end;
  end;
end;

{ TDLExtent }

procedure TDLExtent.ExtendExtent(p: TDLPoint);
begin
  if Assigned(p) then
  begin
    if IsNoWorld then
    begin // init
      xMin := p.x;
      xMax := p.x;
      yMin := p.y;
      yMax := p.y;
    end
    else
    begin // extend to contain point
      if xMin > p.x
      then xMin := p.x;
      if xMax < p.x
      then xMax := p.x;
      if yMin > p.y
      then yMin := p.y;
      if yMax < p.y
      then yMax := p.y;
    end;
  end;
end;

procedure TDLExtent.ExtendExtent(t: TDLTriangle);
begin
  if Assigned(t) then
  begin
    ExtendExtent(t.FVertex1);
    ExtendExtent(t.FVertex2);
    ExtendExtent(t.FVertex3);
  end;
end;

function TDLExtent.ExtentIntersection(const aExtent: TDLExtent): TDLExtent;
begin
  if IsNoWorld or aExtent.IsNoWorld
  then Result := TDLExtent.NoWorld
  else
  begin
    Result := Self;
    if Result.XMin < aExtent.XMin
    then Result.XMin := aExtent.XMin;
    if Result.XMax > aExtent.XMax
    then Result.XMax := aExtent.XMax;
    if Result.YMin < aExtent.YMin
    then Result.YMin := aExtent.YMin;
    if Result.YMax > aExtent.YMax
    then Result.YMax := aExtent.YMax;
  end;
end;

function TDLExtent.ExtentRound: TDLExtent;
begin
  Result.XMin := Floor(xMin);
  Result.YMin := Floor(yMin);
  Result.XMax := Ceil(xMax);
  Result.YMax := Ceil(yMax);
end;

function TDLExtent.ExtentUnion(const aExtent: TDLExtent): TDLExtent;
begin
  if IsNoWorld
  then Result := aExtent
  else
  begin
    Result := Self;
    if Result.XMin > aExtent.XMin
    then Result.XMin := aExtent.XMin;
    if Result.XMax < aExtent.XMax
    then Result.XMax := aExtent.XMax;
    if Result.YMin > aExtent.YMin
    then Result.YMin := aExtent.YMin;
    if Result.YMax < aExtent.YMax
    then Result.YMax := aExtent.YMax;
  end;
end;

function TDLExtent.IsNoWorld: Boolean;
begin
  Result := (xMax < xMin) or (yMax < yMin);
end;

class function TDLExtent.NoWorld: TDLExtent;
begin
  with Result do
  begin
    xMin :=  1;
    xMax := - 1;
    yMin :=  1;
    xMax := - 1;
  end;
end;

function TDLExtent.SameExtent(const aExtent: TDLExtent): Boolean;
begin
  Result :=
    SameValue(XMin, aExtent.XMin) and
    SameValue(YMin, aExtent.YMin) and
    SameValue(XMax, aExtent.XMax) and
    SameValue(YMax, aExtent.YMax);
end;

function TDLExtent.ToString: string;
begin
  Result :=
    FormatFloat('##########.##', xMin, dotFormat) + ';' + FormatFloat('##########.##', yMin, dotFormat)+
    ' - '+
    FormatFloat('##########.##', xMax, dotFormat) + ';' + FormatFloat('##########.##', yMax, dotFormat);
end;

{ TDLPoint }

constructor TDLPoint.Create(ax, ay: TDLCoordinate; aValue: TDLValue);
begin
  inherited Create;
  x := ax;
  y := ay;
  Value := aValue;
end;

function TDLPoint.IsSame(ax, ay: TDLCoordinate): Boolean;
begin
  Result := SameValue(x, ax) and SameValue(y, ay);
end;

function TDLPoint.toString: string;
begin
  Result :=
    FormatFloat('##########.##', x, dotFormat) + ' x ' + FormatFloat('##########.##', y, dotFormat)+': '+value.ToString;
end;

function TDLPoint.contains(aValue: TDLValue): Boolean;
begin
  Result := SameValue(Value, aValue);
end;

{ TDLPointList }

constructor TDLPointList.Create;
begin
  FSortOrder := soUnsorted; // 0 unsorted
  inherited Create;
end;

procedure TDLPointList.SortXY(aThreadPool: TMyThreadPool; aParam: Integer);
// first x then y
begin
  Sort(TComparer<TDLPoint>.Construct(function(const p1, p2: TDLPoint): Integer
    begin
      if p1.x > p2.x
      then Result := 1
      else
      begin
        if p1.x < p2.x
        then Result := -1
        else
        begin
          if p1.y > p2.y
          then Result := 1
          else
          begin
            if p1.y < p2.y
            then Result := -1
            else Result := 0;
          end;
        end;
      end;
    end));
  FSortOrder := soXY; // -1;
end;

procedure TDLPointList.SortYX(aThreadPool: TMyThreadPool; aParam: Integer);
// first y then x
begin
  Sort(TComparer<TDLPoint>.Construct(function(const p1, p2: TDLPoint): Integer
    begin
      if p1.y > p2.y
      then Result := 1
      else
      begin
        if p1.y < p2.y
        then Result := -1
        else
        begin
          if p1.x > p2.x
          then Result := 1
          else
          begin
            if p1.x < p2.x
            then Result := -1
            else Result := 0;
          end;
        end;
      end;
    end));
  FSortOrder := soYX; // 1;
end;

{
function TDLPointList.RemoveDuplicatesXY: Boolean;
var
  i: Integer;
begin
  SortXY;
  // step 2: remove duplicates
  i := 0;
  Result := False;
  while i < Count - 2 do
  begin
    if IsEqualXY(Items[i], Items[i + 1]) then
    begin
      Delete(i + 1); // do not use quickdelete because we have to retain order
      Result := True;
    end
    else inc(i);
  end;
end;
}

procedure TDLPointList.Resort(aThreadPool: TMyThreadPool; aParam: Integer);
begin
  if SortOrder=soXY
  then SortXY(aThreadPool, aParam)
  else if SortOrder=soYX
  then SortYX(aThreadPool, aParam);
end;

procedure TDLPointList.ExtendExtent(var aExtent: TDLExtent);
var
  p: TDLPoint;
begin
  for p in Self
  do aExtent.ExtendExtent(p);
end;

procedure TDLPointList.GetMinMax(expandborderwith: Integer; out minx, miny, maxx, maxy: TDLCoordinate);
var
  guardregion: TDLCoordinate;
begin
  if Count > 0 then
  begin
    SortYX;
    miny := Self[0].y;
    maxy := Self[Count - 1].y;
    SortXY;
    minx := Self[0].x;
    maxx := Self[Count - 1].x;
    // code for expanding the border
    guardregion := (maxy - miny) * (expandborderwith * 0.01); // 1%
    miny := miny - guardregion;
    maxy := maxy + guardregion;
    minx := minx - guardregion;
    maxx := maxx + guardregion;
  end
  else
  begin
    miny := 0;
    maxy := 0;
    minx := 0;
    maxx := 0;
  end;
end;

//function TDLPointList.IndexOf(aID: Integer): Integer;
//begin
//  Result := Count - 1;
//  while (Result >= 0) and (Points[Result].ID <> aID)
//  do Result := Result - 1;
//end;

procedure TDLPointList.QuickDelete(aIndex: Integer);
begin
  // move item to last position so delete does not need to move that much memory
  if aIndex < Count - 1
  then Exchange(aIndex, Count - 1);
  Delete(Count - 1);
  FSortOrder := soUnsorted;
end;

//function TDLPointList.IndexOf(ax, ay: TDLCoordinate): Integer;
//begin
//  Result := Count - 1;
//  while (Result >= 0) and not Points[Result].IsSame(ax, ay)
//  do Result := Result - 1;
//end;

{ TDLTriangle }

{
procedure TDLTriangle.Assign(aTriangle: TDLTriangle);
begin
  FVertex1 := aTriangle.FVertex1;
  FVertex2 := aTriangle.FVertex2;
  FVertex3 := aTriangle.FVertex3;
  FNB1 := aTriangle.FNB1;
  FNB2 := aTriangle.FNB2;
  FNB3 := aTriangle.FNB3;
  FNx := aTriangle.FNx;
  FNy := aTriangle.FNy;
  FNz := aTriangle.FNz;
  NCalculated := aTriangle.NCalculated;
  FDisabled := aTriangle.FDisabled;
end;
}

function TDLTriangle.CheckEdgeLengths(aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate): Boolean;
// true when edge is to long!
begin
  Result :=
    // 1-2
    (Abs(Vertex1.x-Vertex2.x)>aMaxEdgeLengthX) or (Abs(Vertex1.y-Vertex2.y)>aMaxEdgeLengthY) or
    // 2-3
    (Abs(Vertex2.x-Vertex3.x)>aMaxEdgeLengthX) or (Abs(Vertex2.y-Vertex3.y)>aMaxEdgeLengthY) or
    // 3-1
    (Abs(Vertex3.x-Vertex1.x)>aMaxEdgeLengthX) or (Abs(Vertex3.y-Vertex1.y)>aMaxEdgeLengthY);
end;
{
function TDLTriangle.ContainsEdge(p1, p2: TDLPoint): Boolean;
begin
  Result := ((Vertex1 = p1) and (Vertex2 = p2)) or ((Vertex1 = p2) and (Vertex2 = p1)) or
    ((Vertex1 = p1) and (Vertex3 = p2)) or ((Vertex1 = p2) and (Vertex3 = p1)) or ((Vertex2 = p1) and (Vertex3 = p2))
    or ((Vertex2 = p2) and (Vertex3 = p1));
end;

function TDLTriangle.ContainsEdgeInOrder(p1, p2: TDLPoint): Boolean;
begin
  Result := ((Vertex1 = p1) and (Vertex2 = p2)) or ((Vertex2 = p1) and (Vertex3 = p2)) or ((Vertex3 = p1) and (Vertex1 = p2));
end;
}
function TDLTriangle.ContainsPoint(x, y: TDLCoordinate): Boolean;
begin
  if Side(FVertex1, FVertex2, x, y) = sideLeft
  then Result := False
  else
  begin
    if Side(FVertex2, FVertex3, x, y) = sideLeft
    then Result := False
    else
    begin
      if Side(FVertex3, FVertex1, x, y) = sideLeft
      then Result := False
      else Result := True;
    end;
  end;
end;
{
function TDLTriangle.ContainsPoint(aPoint: TDLPoint): Boolean;
begin
  if Side(FVertex1, FVertex2, aPoint) = sideLeft then
    Result := False
  else
  begin
    if Side(FVertex2, FVertex3, aPoint) = sideLeft
    then Result := False
    else
    begin
      if Side(FVertex3, FVertex1, aPoint) = sideLeft
      then Result := False
      else Result := True;
    end;
  end;
end;

function TDLTriangle.ContainsValue(aValue: TDLValue): Boolean;
begin
  Result := False; // sentinel
  // check if one of the vertices contains the given value
  if Assigned(FVertex2) then
  begin
    if not Result
    then Result := EdgeContainsValue(FVertex1, FVertex2, aValue);
    if not Result
    then Result := EdgeContainsValue(FVertex2, FVertex3, aValue);
  end;
  if not Result
  then Result := EdgeContainsValue(FVertex1, FVertex3, aValue);
end;

function TDLTriangle.ContainsVertex(aVertex: TDLPoint): Boolean;
begin
  Result := (FVertex1 = aVertex) or (FVertex2 = aVertex) or (FVertex3 = aVertex);
end;

function TDLTriangle.GetNB(aVertexA, aVertexB: TDLPoint): TDLTriangle;
begin
  if Vertex1 = aVertexA then
  begin
    if Vertex2 = aVertexB
    then Result := NB3
    else Result := nil;
  end
  else
  begin
    if Vertex2 = aVertexA then
    begin
      if Vertex3 = aVertexB
      then Result := NB1
      else Result := nil;
    end
    else
    begin
      if Vertex3 = aVertexA then
      begin
        if Vertex1 = aVertexB
        then Result := NB2
        else Result := nil;
      end
      else Result := nil;
    end;
  end;
end;

function TDLTriangle.GetPOE(aTriangle: TDLTriangle): Integer;
begin
  if FNB1 = aTriangle
  then Result := poeEdge23
  else
  begin
    if FNB2 = aTriangle
    then Result := poeEdge31
    else
    begin
      if FNB3 = aTriangle
      then Result := poeEdge12
      else Result := poeNoEdge;
    end;
  end;
end;
}
function TDLTriangle.HasNoData(aNoData: TDLValue): Boolean;
begin
  if IsNaN(FVertex1.Value) or IsNaN(FVertex2.Value) or IsNaN(FVertex3.Value)
  then Result := True
  else
  begin
    if not IsNaN(aNoData)
    then Result := SameValue(FVertex1.Value, aNoData) or SameValue(FVertex2.Value, aNoData) or SameValue(FVertex3.Value, aNoData)
    else Result := False;
  end;
end;

function TDLTriangle.IsGhost: Boolean;
begin
  Result := not(Assigned(FVertex1) and Assigned(FVertex2) and Assigned(FVertex3));
end;
{
function TDLTriangle.IsOnHull: Boolean;
begin
  Result := not(Assigned(FNB1) and Assigned(FNB2) and Assigned(FNB3));
end;

function TDLTriangle.IsUnlinked: Boolean;
begin
  Result := not(Assigned(FNB1) or Assigned(FNB2) or Assigned(FNB3));
end;

function TDLTriangle.LastLeftTriangle(aCenter: TDLPoint): TDLTriangle;
var
  n: TDLTriangle;
begin
  Result := Self;
  n := Result.NextLeftTriangle(aCenter);
  while Assigned(n) and (n <> Self) do
  begin
    Result := n;
    n := Result.NextLeftTriangle(aCenter);
  end;
end;

function TDLTriangle.LastRightTriangle(aCenter: TDLPoint): TDLTriangle;
var
  n: TDLTriangle;
begin
  Result := Self;
  n := Result.NextRightTriangle(aCenter);
  while Assigned(n) and (n <> Self) do
  begin
    Result := n;
    n := Result.NextRightTriangle(aCenter);
  end;
end;

function TDLTriangle.LengthOfLongestEdge: TDLCoordinate;
var
  d12: TDLCoordinate;
  d23: TDLCoordinate;
  d31: TDLCoordinate;
begin
  d12 := Distance(Vertex1, Vertex2);
  d23 := Distance(Vertex2, Vertex3);
  d31 := Distance(Vertex3, Vertex1);
  Result := Max(d12, Max(d23, d31));
end;
}
function TDLTriangle.LinkTo(aTriangle: TDLTriangle; aVertexA, aVertexB: TDLPoint): Integer;
begin
  if (Vertex2 = aVertexA) and (Vertex3 = aVertexB) then
  begin
    FNB1 := aTriangle;
    Result := poeEdge23;
  end
  else
  begin
    if (Vertex3 = aVertexA) and (Vertex1 = aVertexB) then
    begin
      FNB2 := aTriangle;
      Result := poeEdge31;
    end
    else
    begin
      if (Vertex1 = aVertexA) and (Vertex2 = aVertexB) then
      begin
        FNB3 := aTriangle;
        Result := poeEdge12;
      end
      else Result := poeNoEdge;
    end;
  end;
end;
{
function TDLTriangle.NextHullEdge(var poe: Integer): TDLTriangle;
begin
  Result := Self; // sentinel: we stay within this triangle
  case poe of
    poeEdge12:
      if Assigned(NB1) then
      begin // find next hull triangle, rotating CCW around Vertex2
        Result := NB1.LastLeftTriangle(Vertex2);
        // find poe on hull starting with this vertex of this triangle
        if Result.Vertex1 = Vertex2
        then poe := poeEdge12
        else
        begin
          if Result.Vertex2 = Vertex2
          then poe := poeEdge23
          else poe := poeEdge31;
        end;
      end
      else
        poe := poeEdge23;
    poeEdge23:
      if Assigned(NB2) then
      begin // find next hull triangle, rotating CCW around Vertex3
        Result := NB2.LastLeftTriangle(Vertex3);
        // find poe on hull starting with this vertex of this triangle
        if Result.Vertex1 = Vertex3
        then poe := poeEdge12
        else
        begin
          if Result.Vertex2 = Vertex3
          then poe := poeEdge23
          else poe := poeEdge31;
        end;
      end
      else
        poe := poeEdge31;
    poeEdge31:
      if Assigned(NB3) then
      begin // find next hull triangle, rotating CCW around Vertex1
        Result := NB3.LastLeftTriangle(Vertex1);
        // find poe on hull starting with this vertex of this triangle
        if Result.Vertex1 = Vertex1
        then poe := poeEdge12
        else
        begin
          if Result.Vertex2 = Vertex1
          then poe := poeEdge23
          else poe := poeEdge31;
        end;
      end
      else
        poe := poeEdge12;
  else // poeNoEdge: find first edge
    if Assigned(NB1) then
    begin
      if Assigned(NB2) then
      begin
        if Assigned(NB3) then
        begin
          poe := poeNoEdge;
          Result := nil;
        end
        else poe := poeEdge12;
      end
      else poe := poeEdge31;
    end
    else poe := poeEdge23;
  end;
end;
}
function TDLTriangle.NextLeftTriangle(aCenter: TDLPoint): TDLTriangle;
begin
  if aCenter = FVertex1
  then Result := FNB3
  else
  begin
    if aCenter = FVertex2
    then Result := FNB1
    else Result := FNB2;
  end;
end;

function TDLTriangle.NextRightTriangle(aCenter: TDLPoint): TDLTriangle;
begin
  if aCenter = FVertex1
  then Result := FNB2
  else
  begin
    if aCenter = FVertex2
    then Result := FNB3
    else Result := FNB1;
  end;
end;
{
function TDLTriangle.poeToVertexCCW(poe: Integer): TDLPoint;
begin
  case poe of
    poeEdge12:
      Result := Vertex1;
    poeEdge23:
      Result := Vertex2;
    poeEdge31:
      Result := Vertex3;
  else
      Result := nil;
  end;
end;

function TDLTriangle.poeToVertexCW(poe: Integer): TDLPoint;
begin
  case poe of
    poeEdge12:
      Result := Vertex2;
    poeEdge23:
      Result := Vertex3;
    poeEdge31:
      Result := Vertex1;
  else
      Result := nil;
  end;
end;

function TDLTriangle.PointOnEdge(aPoint: TDLPoint): Integer;
begin
//   asume point is within triangle, now check if on edge
  if Side(FVertex1, FVertex2, aPoint) = sideColineair
  then Result := poeEdge12
  else
  begin
    if Side(FVertex2, FVertex3, aPoint) = sideColineair
    then Result := poeEdge23
    else
    begin
      if Side(FVertex3, FVertex1, aPoint) = sideColineair
      then Result := poeEdge31
      else Result := poeNoEdge;
    end;
  end;
end;

function TDLTriangle.PrevHullEdge(var poe: Integer): TDLTriangle;
begin
  Result := Self; // sentinel: we stay within this triangle
  case poe of
    poeEdge12:
      if Assigned(NB2) then
      begin // find next hull triangle, rotating CW around Vertex2
        Result := NB2.LastRightTriangle(Vertex1);
        // find poe on hull starting with this vertex of this triangle
        if Result.Vertex1 = Vertex1
        then poe := poeEdge31
        else
        begin
          if Result.Vertex2 = Vertex1
          then poe := poeEdge12
          else poe := poeEdge23;
        end;
      end
      else poe := poeEdge31;
    poeEdge23:
      if Assigned(NB3) then
      begin // find next hull triangle, rotating CW around Vertex3
        Result := NB3.LastRightTriangle(Vertex2);
        // find poe on hull starting with this vertex of this triangle
        if Result.Vertex1 = Vertex2
        then poe := poeEdge31
        else
        begin
          if Result.Vertex2 = Vertex2
          then poe := poeEdge12
          else poe := poeEdge23;
        end;
      end
      else
        poe := poeEdge12;
    poeEdge31:
      if Assigned(NB1) then
      begin // find next hull triangle, rotating CW around Vertex1
        Result := NB1.LastRightTriangle(Vertex3);
        // find poe on hull starting with this vertex of this triangle
        if Result.Vertex1 = Vertex3
        then poe := poeEdge31
        else
        begin
          if Result.Vertex2 = Vertex3
          then poe := poeEdge12
          else poe := poeEdge23;
        end;
      end
      else poe := poeEdge23;
  else // poeNoEdge: find first edge
    if Assigned(NB1) then
    begin
      if Assigned(NB2) then
      begin
        if Assigned(NB3) then
        begin
          poe := poeNoEdge;
          Result := nil;
        end
        else poe := poeEdge12;
      end
      else poe := poeEdge31;
    end
    else poe := poeEdge23;
  end;
end;

procedure TDLTriangle.Relink(aSrcTriangle, aDstTriangle: TDLTriangle);
begin
  if FNB1 = aSrcTriangle
  then FNB1 := aDstTriangle;
  if FNB2 = aSrcTriangle
  then FNB2 := aDstTriangle;
  if FNB3 = aSrcTriangle
  then FNB3 := aDstTriangle;
end;
}
procedure TDLTriangle.ShiftNil;
var
  tnb1: TDLTriangle;
begin
  // put the nil point in the right place, that is the second position
  if Assigned(FVertex2) then
  begin
    if not Assigned(FVertex1) then
    begin
      FVertex1 := FVertex3;
      FVertex3 := FVertex2;
      FVertex2 := nil;
      tnb1 := FNB1;
      FNB1 := FNB3;
      FNB3 := FNB2;
      FNB2 := tnb1;
    end
    else
    begin // thus vertex3 = nil
      FVertex3 := FVertex1;
      FVertex1 := FVertex2;
      FVertex2 := nil;
      tnb1 := FNB1;
      FNB1 := FNB2;
      FNB2 := FNB3;
      FNB3 := tnb1;
    end;
  end;
end;

procedure TDLTriangle.Unlink;
begin
  if Assigned(FNB1) then
  begin
    FNB1.UnlinkTo(Self);
    FNB1 := nil;
  end;
  if Assigned(FNB2) then
  begin
    FNB2.UnlinkTo(Self);
    FNB2 := nil;
  end;
  if Assigned(FNB3) then
  begin
    FNB3.UnlinkTo(Self);
    FNB3 := nil;
  end;
end;

procedure TDLTriangle.UnlinkTo(aTriangle: TDLTriangle);
begin
  if FNB1 = aTriangle
  then FNB1 := nil;
  if FNB2 = aTriangle
  then FNB2 := nil;
  if FNB3 = aTriangle
  then FNB3 := nil;
end;
{
function TDLTriangle.ValueOnPoint(aPoint: TDLPoint): TDLValue;
begin
  if not NCalculated then
  begin
    // N := (Vertex3-Vertex1)x(Vertex2-Vertex1)
    // N.x = b.y*c.z - b.z*c.y
    // N.y = b.z*c.x - b.x*c.z
    // N.z = b.x*c.y - b.y*c.x
    FNx := (Vertex3.y - Vertex1.y) * (Vertex2.Value - Vertex1.Value) - (Vertex3.Value - Vertex1.Value) * (Vertex2.y - Vertex1.y);
    FNy := (Vertex3.Value - Vertex1.Value) * (Vertex2.x - Vertex1.x) - (Vertex3.x - Vertex1.x) * (Vertex2.Value - Vertex1.Value);
    FNz := (Vertex3.x - Vertex1.x) * (Vertex2.y - Vertex1.y) - (Vertex3.y - Vertex1.y) * (Vertex2.x - Vertex1.x);
    NCalculated := True;
  end;
  Result := (FNx*(aPoint.X-Vertex1.X)+FNy*(aPoint.Y-Vertex1.Y))/-FNz+Vertex1.Value;
end;
}
function TDLTriangle.ValueOnPoint(x, y: TDLCoordinate): TDLValue;
begin
  NCalculated:=false; //tmp hack, should be made better....
  if not NCalculated then
  begin
    // N := (Vertex3-Vertex1)x(Vertex2-Vertex1)
    // N.x = b.y*c.z - b.z*c.y
    // N.y = b.z*c.x - b.x*c.z
    // N.z = b.x*c.y - b.y*c.x
    FNx := (Vertex3.y - Vertex1.y) * (Vertex2.Value - Vertex1.Value) - (Vertex3.Value - Vertex1.Value) * (Vertex2.y - Vertex1.y);
    FNy := (Vertex3.Value - Vertex1.Value) * (Vertex2.x - Vertex1.x) - (Vertex3.x - Vertex1.x) * (Vertex2.Value - Vertex1.Value);
    FNz := (Vertex3.x - Vertex1.x) * (Vertex2.y - Vertex1.y) - (Vertex3.y - Vertex1.y) * (Vertex2.x - Vertex1.x);
    NCalculated := True;
  end;
  Result := (FNx*(X-Vertex1.X)+FNy*(Y-Vertex1.Y))/-FNz+Vertex1.Value;
end;

{ TDLTriangulation }

{
function TDLTriangleList.CheckConsistency: Boolean;
var
  t: Integer;
begin
  // all triangles must have a valid nb1, nb2 and nb3 (registered triangle)
  // no triangle is allowed to be a ghost
  // all assigned triangle neighbour links should link back to referencing triangle
  Result := True;
  for t := 0 to Count - 1 do
  begin
    if Triangles[t].IsGhost
    then Result := False;
    if (Count > 1) and Triangles[t].IsUnlinked
    then Result := False;
    if Assigned(Triangles[t].NB1) then
    begin
      if IndexOf(Triangles[t].NB1) < 0
      then Result := False;
      if Triangles[t].NB1.GetNB(Triangles[t].Vertex3, Triangles[t].Vertex2) <> Triangles[t]
      then Result := False;
    end;
    if Assigned(Triangles[t].NB2) then
    begin
      if IndexOf(Triangles[t].NB2) < 0
      then Result := False;
      if Triangles[t].NB2.GetNB(Triangles[t].Vertex1, Triangles[t].Vertex3) <> Triangles[t]
      then Result := False;
    end;
    if Assigned(Triangles[t].NB3) then
    begin
      if IndexOf(Triangles[t].NB3) < 0
      then Result := False;
      if Triangles[t].NB3.GetNB(Triangles[t].Vertex2, Triangles[t].Vertex1) <> Triangles[t]
      then Result := False;
    end;
  end;
end;

function TDLTriangleList.CheckUnlinked(aExceptions: TDLTriangleList): Boolean;
var
  t: Integer;
begin
  // no triangle is allowed to be unlinked (1 mesh)
  Result := True;
  for t := 0 to Count - 1 do
  begin
    if Triangles[t].IsUnlinked and ((not Assigned(aExceptions)) or (aExceptions.IndexOf(Triangles[t]) < 0))
    then Result := False;
  end;
end;

function TDLTriangleList.ContainsGhostTriangle: Boolean;
var
  t: Integer;
begin
  t := Count - 1;
  while (t >= 0) and not Triangles[t].IsGhost
  do t := t - 1;
  Result := t >= 0;
end;

function TDLTriangleList.ContainsVertex(aVertex: TDLPoint): Boolean;
var
  t: Integer;
begin
  // find all triangles that contain (as vertex) given point
  t := Count - 1;
  while (t >= 0) and not Triangles[t].ContainsVertex(aVertex)
  do t := t - 1;
  Result := t >= 0;
end;
}
constructor TDLTriangleList.Create(aOwnsTriangles: Boolean);
begin
  FOwnsTriangles := aOwnsTriangles;
  inherited Create;
end;

function TDLTriangleList.DisableTooLongEdgesAndNoData(aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; aNodata: TDLValue): Boolean;
var
  t: Integer;
begin
  Result := False;
  for t := 0 to Count - 1 do
  begin
    if (not Triangles[t].Disabled) and
       (Triangles[t].CheckEdgeLengths(aMaxEdgeLengthX, aMaxEdgeLengthY) or
        Triangles[t].HasNoData(aNoData)) then
    begin
      Triangles[t].Disabled := True;
      Result := True;
    end;
  end;
end;
{
function TDLTriangleList.FindFirstHullTriangle: TDLTriangle;
var
  t: Integer;
begin
  t := 0;
  while (t < Count) and not Triangles[t].IsOnHull
  do t := t + 1;
  if t < Count
  then Result := Triangles[t]
  else Result := nil;
end;

function TDLTriangleList.FindFirstTriangle(aVertex: TDLPoint): TDLTriangle;
var
  t: Integer;
begin
  t := 0;
  while (t < Count) and not Triangles[t].ContainsVertex(aVertex)
  do t := t + 1;
  if t < Count
  then Result := Triangles[t]
  else Result := nil;
end;

procedure TDLTriangleList.FindTriangles(aVertex: TDLPoint; aTriangles: TList);
var
  t: Integer;
begin
  // find all triangles that contain (as vertex) given point
  for t := 0 to Count - 1 do
  begin
    if Triangles[t].ContainsVertex(aVertex)
    then aTriangles.add(Triangles[t]);
  end;
end;
}
function TDLTriangleList.GetTriangle(aIndex: Integer): TDLTriangle;
begin
  Result := TDLTriangle(Items[aIndex]);
end;
{
procedure TDLTriangleList.MarkAsUncalculated(aPoint: TDLPoint; aAffectedTriangles: TList);
var
  t: Integer;
begin
  // find all triangles that contain (as vertex) given point
  for t := 0 to Count - 1 do
  begin
    if Triangles[t].ContainsVertex(aPoint) then
    begin
      Triangles[t].NCalculated := False;
      if Assigned(aAffectedTriangles)
      then aAffectedTriangles.add(Triangles[t]);
    end;
  end;
end;
}
procedure TDLTriangleList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and FOwnsTriangles
  then TDLTriangle(Ptr).Free;
end;
{
function TDLTriangleList.PointToTriangle(aPoint: TDLPoint): TDLTriangle;
var
  t: Integer;
begin
  t := Count - 1;
  while (t >= 0) and not Triangles[t].ContainsPoint(aPoint)
  do t := t - 1;
  if t >= 0
  then Result := Triangles[t]
  else Result := nil;
end;

function TDLTriangleList.PointToValue(aPoint: TDLPoint; aNoValue: TDLValue): TDLValue;
var
  t: Integer;
begin
  t := Count - 1;
  while (t >= 0) and not Triangles[t].ContainsPoint(aPoint)
  do t := t - 1;
  if t >= 0
  then Result := Triangles[t].ValueOnPoint(aPoint)
  else Result := aNoValue;
end;
}
function TDLTriangleList.PointToValue(x, y: TDLCoordinate; aNoValue: TDLValue): TDLValue;
var
  t: Integer;
begin
  t := Count - 1;
  while (t >= 0) and not Triangles[t].ContainsPoint(x, y)
  do t := t - 1;
  if t >= 0
  then Result := Triangles[t].ValueOnPoint(x, y)
  else Result := aNoValue;
end;

procedure TDLTriangleList.QuickDelete(aIndex: Integer);
begin
  // move item to last position so delete does not need to move that much memory
  if aIndex < Count - 1
  then Exchange(aIndex, Count - 1);
  Delete(Count - 1);
end;
{
function TDLTriangleList.QuickRemove(aItem: Pointer): Integer;
begin
  Result := IndexOf(aItem);
  if Result >= 0
  then QuickDelete(Result);
end;
}
procedure TDLTriangleList.RemoveGhostTriangles;
var
  t: Integer;
begin
  t := 0;
  while t < Count do
  begin
    if Triangles[t].IsGhost then
    begin
      Triangles[t].Unlink;
      QuickDelete(t);
    end
    else t := t + 1;
  end;
end;

{ TDLNode }

procedure TDLNode.Clear;
begin
  FreeAndNil(FTriangles);
  FreeAndNil(FChild1);
  FreeAndNil(FChild2);
  if Assigned(FPoints)
  then FPoints.Clear;
end;

constructor TDLNode.Create(aPoints: TDLPointList);
begin
  inherited Create;
  FChild1 := nil;
  FChild2 := nil;
  if Assigned(aPoints)
  then FPoints := aPoints
  else FPoints := TDLPointList.Create;
  FTriangles := nil;
end;

destructor TDLNode.Destroy;
begin
  FreeAndNil(FTriangles);
  FreeAndNil(FPoints);
  FreeAndNil(FChild1);
  FreeAndNil(FChild2);
  inherited Destroy;
end;

procedure TDLNode.DevidePoints(aThreadPool: TMyThreadPool; aParam: Integer);
var
  k: Integer;
  p: Integer;
  EndEvent: TMyThreadPoolEndEvent;
begin
  if Assigned(Points) and (Points.Count > 3) then
  begin
    // create 2 child nodes
    // first devide the point list
    k := (Points.Count div 2) - 1;
    // add points in higher part of point list to child 2
    FChild2 := TDLNode.Create(nil); // new point list
    FChild2.Points.Capacity := Points.Count-k-1; // (Points.Count - 1) - (k + 1) + 1
    for p := k + 1 to Points.Count - 1
    do FChild2.Points.add(Points[p]);
    // shrink current point list to lower part and move point list to child 1
    Points.Count := k+1; // (k) - (0) + 1
    FChild1 := TDLNode.Create(FPoints); // move ownership of current point list to child 1
    // we no longer own the point list
    FPoints := nil;
    // sort the new lists in the opposite order
    if Assigned(aThreadPool) and (aParam>0) then
    begin
      if FChild1.Points.SortOrder = soXY { -1 } then
      begin
        EndEvent := aThreadPool.CreateEndEvent(1);
        try
          // start sorting and deviding in second thread
          aThreadPool.Enqueue([FChild2.Points.SortYX, FChild2.DevidePoints], [aParam-1, aParam-1], EndEvent);
          // we continue sorting and deviding child 1 our selves
          FChild1.Points.SortYX(aThreadPool, aParam-1);
          FChild1.DevidePoints(aThreadPool, aParam-1);
          // wait for second thread to finish
          EndEvent.WaitFor;
        finally
          EndEvent.Free;
        end;
      end
      else
      begin
        EndEvent := aThreadPool.CreateEndEvent(1);
        try
          // start sorting and deviding in second thread
          aThreadPool.Enqueue([FChild2.Points.SortXY, FChild2.DevidePoints], [aParam-1, aParam-1], EndEvent);
          // we continue sorting and deviding child 1
          FChild1.Points.SortXY(aThreadPool, aParam-1);
          FChild1.DevidePoints(aThreadPool, aParam-1);
          // wait for second thread to finish
          EndEvent.WaitFor;
        finally
          EndEvent.Free;
        end;
      end;
    end
    else
    begin
      if FChild1.Points.SortOrder = soXY { -1 } then
      begin
        FChild1.Points.SortYX(aThreadPool, aParam-1);
        FChild2.Points.SortYX(aThreadPool, aParam-1);
      end
      else
      begin
        FChild1.Points.SortXY(aThreadPool, aParam-1);
        FChild2.Points.SortXY(aThreadPool, aParam-1);
      end;
      FChild1.DevidePoints(aThreadPool, aParam-1);
      FChild2.DevidePoints(aThreadPool, aParam-1);
    end;
  end;
end;

procedure TDLNode.Merge(aThreadPool: TMyThreadPool; aParam: Integer);
var
  t: Integer;
  EndEvent: TMyThreadPoolEndEvent;
begin
  // merge from bottom to top because the merge is always upward in the tree
  // there are items without children: the leaves !
  if Assigned(Child1) then
  begin
    // there are always 2 children so if they both have points, merge the 2
    // first merge the children
    if Assigned(aThreadPool) and (aParam>0) then
    begin
      EndEvent := aThreadPool.CreateEndEvent(1);
      try
        aThreadPool.Enqueue(Child1.Merge, aParam-1, EndEvent);
        Child2.Merge(aThreadPool, aParam-1);
        EndEvent.WaitFor;
      finally
        EndEvent.Free;
      end;
    end
    else
    begin
      Child1.Merge(aThreadPool, aParam-1);
      Child2.Merge(aThreadPool, aParam-1);
    end;
    // merge triangles of children with triangles of this node
    if Assigned(FTriangles)
    then FTriangles.Clear
    else FTriangles := TDLTriangleList.Create(False);
    // pass the three triangles objects to the mergeTB procedure
    MergeLR(Triangles, Child1.Triangles, Child2.Triangles);
    // now add the children triangles to the parent and delete the children nodes
    Triangles.Capacity := Triangles.Count+Child1.Triangles.Count+Child2.Triangles.Count;
    for t := 0 to Child1.Triangles.Count - 1
    do Triangles.add(Child1.Triangles[t]);
    for t := 0 to Child2.Triangles.Count - 1
    do Triangles.add(Child2.Triangles[t]);
    FreeAndNil(FChild1);
    FreeAndNil(FChild2);
  end;
end;

procedure TDLNode.RemoveGhostsTriangles;
begin
  if Assigned(Triangles)
  then Triangles.RemoveGhostTriangles;
  if Assigned(Child1)
  then Child1.RemoveGhostsTriangles;
  if Assigned(Child2)
  then Child2.RemoveGhostsTriangles;
end;

procedure TDLNode.Triangulate(aThreadPool: TMyThreadPool; aParam: Integer);
var
  EndEvent: TMyThreadPoolEndEvent;
begin
  if Assigned(Points) then
  begin
    if Assigned(FTriangles)
    then FTriangles.Clear
    else FTriangles := TDLTriangleList.Create(False);
    if Points.Count = 2 then
    begin
      // 2 nodes -> 2 triangles
      Triangles.add(TDLTriangle.Create);
      Triangles.add(TDLTriangle.Create);
      with Triangles[0] do
      begin
        // clockwise triangle 1
        FVertex1 := Points[1];
        FVertex2 := nil;
        FVertex3 := Points[0];
        FNB1 := Triangles[1];
        FNB2 := Triangles[1];
        FNB3 := Triangles[1];
        // nb2 :
      end;
      with Triangles[1] do
      begin
        // clockwise triangle 2
        FVertex1 := Points[0];
        FVertex2 := nil;
        FVertex3 := Points[1];
        FNB1 := Triangles[0];
        FNB2 := Triangles[0];
        FNB3 := Triangles[0];
      end;
    end
    else
    begin
      // always 4 triangles, but different structure for colineair points
      // so create them
      Triangles.add(TDLTriangle.Create);
      Triangles.add(TDLTriangle.Create);
      Triangles.add(TDLTriangle.Create);
      Triangles.add(TDLTriangle.Create);
      if Side(Points[1], Points[2], Points[0]) = sideColineair then
      begin
        // colineair. sort on y and x or x and y so we can
        // always use the order p1 -> p2 and p2 -> p3
        with Triangles[0] do
        begin
          // clockwise triangle 1
          FVertex1 := Points[0];
          FVertex2 := nil;
          FVertex3 := Points[1];
          FNB1 := Triangles[1];
          FNB2 := Triangles[3];
          FNB3 := Triangles[3];
        end;
        with Triangles[1] do
        begin
          // clockwise triangle 2
          FVertex1 := Points[1];
          FVertex2 := nil;
          FVertex3 := Points[2];
          FNB1 := Triangles[2];
          FNB2 := Triangles[2];
          FNB3 := Triangles[0];
        end;
        with Triangles[2] do
        begin
          // clockwise triangle 1
          FVertex1 := Points[2];
          FVertex2 := nil;
          FVertex3 := Points[1];
          FNB1 := Triangles[3];
          FNB2 := Triangles[1];
          FNB3 := Triangles[1];
        end;
        with Triangles[3] do
        begin
          // clockwise triangle 2
          FVertex1 := Points[1];
          FVertex2 := nil;
          FVertex3 := Points[0];
          FNB1 := Triangles[0];
          FNB2 := Triangles[0];
          FNB3 := Triangles[2];
        end;
      end
      else
      begin
        // not colineair
        if Side(Points[1], Points[2], Points[0]) = sideRight { !! } then
        begin
          // p3 is clockwise from 1 -> 2, maintain current order
          with Triangles[0] do
          begin
            // clockwise triangle 1
            FVertex1 := Points[0];
            FVertex2 := Points[1];
            FVertex3 := Points[2];
            FNB1 := Triangles[2];
            FNB2 := Triangles[3];
            FNB3 := Triangles[1];
            // nb2 :
          end;
          with Triangles[1] do
          begin
            // clockwise triangle 2
            FVertex1 := Points[0];
            FVertex2 := nil;
            FVertex3 := Points[1];
            FNB1 := Triangles[2];
            FNB2 := Triangles[0];
            FNB3 := Triangles[3];
          end;
          with Triangles[2] do
          begin
            // clockwise triangle 1
            FVertex1 := Points[1];
            FVertex2 := nil;
            FVertex3 := Points[2];
            FNB1 := Triangles[3];
            FNB2 := Triangles[0];
            FNB3 := Triangles[1];
            // nb2 :
          end;
          with Triangles[3] do
          begin
            // clockwise triangle 2
            FVertex1 := Points[2];
            FVertex2 := nil;
            FVertex3 := Points[0];
            FNB1 := Triangles[1];
            FNB2 := Triangles[0];
            FNB3 := Triangles[2];
          end;
        end
        else
        begin
          // p3 is anticlockwise from 1 -> 2
          with Triangles[0] do
          begin
            // clockwise triangle 1
            FVertex1 := Points[0];
            FVertex2 := Points[2];
            FVertex3 := Points[1];
            FNB1 := Triangles[2];
            FNB2 := Triangles[3];
            FNB3 := Triangles[1];
          end;
          with Triangles[1] do
          begin
            // clockwise triangle 2
            FVertex1 := Points[0];
            FVertex2 := nil;
            FVertex3 := Points[2];
            FNB1 := Triangles[2];
            FNB2 := Triangles[0];
            FNB3 := Triangles[3];
          end;
          with Triangles[2] do
          begin
            // clockwise triangle 1
            FVertex1 := Points[2];
            FVertex2 := nil;
            FVertex3 := Points[1];
            FNB1 := Triangles[3];
            FNB2 := Triangles[0];
            FNB3 := Triangles[1];
          end;
          with Triangles[3] do
          begin
            // clockwise triangle 2
            FVertex1 := Points[1];
            FVertex2 := nil;
            FVertex3 := Points[0];
            FNB1 := Triangles[1];
            FNB2 := Triangles[0];
            FNB3 := Triangles[2];
          end;
        end;
      end;
    end;
  end
  else
  begin
    if Assigned(aThreadPool) and (aParam>0) and Assigned(Child1) and Assigned(Child1) then
    begin
      EndEvent := aThreadPool.CreateEndEvent(1);
      try
        aThreadPool.Enqueue(Child1.Triangulate, aParam-1, EndEvent);
        Child2.Triangulate(aThreadPool, aParam-1);
        EndEvent.WaitFor;
      finally
        EndEvent.Free;
      end;
    end
    else
    begin
      if Assigned(Child1)
      then Child1.Triangulate(aThreadPool, aParam-1);
      if Assigned(Child2)
      then Child2.Triangulate(aThreadPool, aParam-1);
    end;
  end;
end;

function CanFormCWTriangle(aTriangle: TDLTriangle; poe: Integer; aPoint: TDLPoint): Boolean;
begin
  // poe is edge that should be used in reverse as base for new triangle
  case poe of
    poeEdge12:
      Result := Side(aTriangle.Vertex2, aTriangle.Vertex1, aPoint) = sideRight;
    poeEdge23:
      Result := Side(aTriangle.Vertex3, aTriangle.Vertex2, aPoint) = sideRight;
    poeEdge31:
      Result := Side(aTriangle.Vertex1, aTriangle.Vertex3, aPoint) = sideRight;
  else
    Result := False;
  end;
end;

function CreateCWTriangle(aTriangle: TDLTriangle; poe: Integer; aPoint: TDLPoint): TDLTriangle;
begin
  // poe is edge that should be used in reverse as base for new triangle
  case poe of
    poeEdge12:
      begin
        Result := TDLTriangle.Create;
        Result.FVertex1 := aTriangle.Vertex1;
        Result.FVertex2 := aPoint;
        Result.FVertex3 := aTriangle.Vertex2;
        Result.FNB2 := aTriangle;
        aTriangle.FNB3 := Result;
      end;
    poeEdge23:
      begin
        Result := TDLTriangle.Create;
        Result.FVertex1 := aTriangle.Vertex2;
        Result.FVertex2 := aPoint;
        Result.FVertex3 := aTriangle.Vertex3;
        Result.FNB2 := aTriangle;
        aTriangle.FNB1 := Result;
      end;
    poeEdge31:
      begin
        Result := TDLTriangle.Create;
        Result.FVertex1 := aTriangle.Vertex3;
        Result.FVertex2 := aPoint;
        Result.FVertex3 := aTriangle.Vertex1;
        Result.FNB2 := aTriangle;
        aTriangle.FNB2 := Result;
      end;
  else
    Result := nil;
  end;
end;
{
function CheckHull(aTriangleList: TDLTriangleList): Boolean;
var
  S, t: TDLTriangle;
  poeS, poeT: Integer;
begin
  S := aTriangleList.FindFirstHullTriangle;
  if Assigned(S) then
  begin
    poeS := poeNoEdge;
    S := S.NextHullEdge(poeS);
    if Assigned(S) and (poeS <> poeNoEdge) then
    begin
      poeT := poeS;
      t := S.NextHullEdge(poeT);
      if (t <> S) or (poeS <> poeT) then
      begin
        while (t <> S) or (poeS <> poeT)
        do t := t.NextHullEdge(poeT);
        Result := (t = S) and (poeS = poeT);
      end
      else Result := False;
    end
    else Result := False;
  end
  else Result := False;
end;
}
{ TDelaunyNet }
{
procedure TDLNet.AbortTriangulation();
begin
  FTriangles.Clear;
end;

procedure TDLNet.AddPoint(aPoint: TDLPoint; aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; aNoData: TDLValue);

  function SplitTriangle(aTriangle: TDLTriangle; aPoint: TDLPoint; poe: Integer; out aNB: TDLTriangle): TDLTriangle;
  begin
    case poe of
      // split triangles with edges that contains this point and add 2 triangles for each
      poeEdge12:
        begin
          // create copy of triangle
          Result := TDLTriangle.Create;
          Result.Assign(aTriangle);
          // put point in both triangles on correct position
          aTriangle.FVertex2 := aPoint;
          Result.FVertex1 := aPoint;
          // fix neighbours
          if Assigned(aTriangle.FNB1)
          then aTriangle.FNB1.Relink(aTriangle, Result);
          aTriangle.FNB1 := Result;
          Result.FNB2 := aTriangle;
          // mark as not calculated
          aTriangle.NCalculated := False;
          Result.NCalculated := False;
          // add Result to all lists
          Triangles.add(Result);
          aNB := aTriangle.NB3;
        end;
      poeEdge23:
        begin
          Result := TDLTriangle.Create;
          Result.Assign(aTriangle);
          aTriangle.FVertex3 := aPoint;
          Result.FVertex2 := aPoint;
          if Assigned(aTriangle.FNB2)
          then aTriangle.FNB2.Relink(aTriangle, Result);
          aTriangle.FNB2 := Result;
          Result.FNB3 := aTriangle;
          aTriangle.NCalculated := False;
          Result.NCalculated := False;
          // add Result to all lists
          Triangles.add(Result);
          aNB := aTriangle.NB1;
        end;
      poeEdge31:
        begin
          Result := TDLTriangle.Create;
          Result.Assign(aTriangle);
          aTriangle.FVertex1 := aPoint;
          Result.FVertex3 := aPoint;
          if Assigned(aTriangle.FNB3)
          then aTriangle.FNB3.Relink(aTriangle, Result);
          aTriangle.FNB3 := Result;
          Result.FNB1 := aTriangle;
          aTriangle.NCalculated := False;
          Result.NCalculated := False;
          // add Result to all lists
          Triangles.add(Result);
          aNB := aTriangle.NB2;
        end;
    else
      Result := nil;
      aNB := nil;
    end;
  end;

var
  Triangle: TDLTriangle;
  poe: Integer;
  NB: TDLTriangle;
  i: Integer;
  AffectedTriangles: TDLTriangleList;
  NewTriangle: TDLTriangle;
  NextTriangle: TDLTriangle;
  FirstTriangle: TDLTriangle;
  SecondTriangle: TDLTriangle;
  ThirdTriangle: TDLTriangle;
  NB2: TDLTriangle;
  PrevTriangle: TDLTriangle;
  poePrev: Integer;
  LastTriangle: TDLTriangle;
  poeNext: Integer;
begin
  i := Points.IndexOf(aPoint.x, aPoint.y); // todo: take sorting into account
  if i < 0 then
  begin
    // add point to owned points list
    Points.add(aPoint); // todo: take sorting into account
    if Assigned(Triangles) then
    begin
      if Triangles.Count > 0 then
      begin
        Triangle := Triangles.PointToTriangle(aPoint);
        if Assigned(Triangle) then
        begin
          // check if on a edge
          poe := Triangle.PointOnEdge(aPoint);
          if poe = poeNoEdge then
          begin
            // triangulate within triangle
            // create 3 new triangles that have new point as vertex2, NB2 points to edges of old triangle
            // edge12 of Triangle
            FirstTriangle := TDLTriangle.Create;
            FirstTriangle.FVertex1 := Triangle.Vertex2;
            FirstTriangle.FVertex2 := aPoint;
            FirstTriangle.FVertex3 := Triangle.Vertex1;
            // edge23 of Triangle
            SecondTriangle := TDLTriangle.Create;
            SecondTriangle.FVertex1 := Triangle.Vertex3;
            SecondTriangle.FVertex2 := aPoint;
            SecondTriangle.FVertex3 := Triangle.Vertex2;
            // edge31 of Triangle
            ThirdTriangle := TDLTriangle.Create;
            ThirdTriangle.FVertex1 := Triangle.Vertex1;
            ThirdTriangle.FVertex2 := aPoint;
            ThirdTriangle.FVertex3 := Triangle.Vertex3;
            // link missed NBs
            FirstTriangle.FNB1 := ThirdTriangle;
            FirstTriangle.FNB2 := Triangle.NB3;
            if Assigned(FirstTriangle.FNB2)
            then FirstTriangle.FNB2.Relink(Triangle, FirstTriangle);
            FirstTriangle.FNB3 := SecondTriangle;
            SecondTriangle.FNB1 := FirstTriangle;
            SecondTriangle.FNB2 := Triangle.NB1;
            if Assigned(SecondTriangle.FNB2)
            then SecondTriangle.FNB2.Relink(Triangle, SecondTriangle);
            SecondTriangle.FNB3 := ThirdTriangle;
            ThirdTriangle.FNB1 := SecondTriangle;
            ThirdTriangle.FNB2 := Triangle.NB2;
            if Assigned(ThirdTriangle.FNB2) then
              ThirdTriangle.FNB2.Relink(Triangle, ThirdTriangle);
            ThirdTriangle.FNB3 := FirstTriangle;
            // add new triangles to global list
            Triangles.add(FirstTriangle);
            Triangles.add(SecondTriangle);
            Triangles.add(ThirdTriangle);
            // all is linked now, remove old triangle
            Triangle.FNB1 := nil;
            Triangle.FNB2 := nil;
            Triangle.FNB3 := nil;
            Triangles.QuickRemove(Triangle);
          end
          else
          begin
            // todo: update aIntermetiateGrid!
            // split triangles with edges that contains this point and add 2 triangles for each
            SecondTriangle := SplitTriangle(Triangle, aPoint, poe, NB);
            // check for oposite triangle on edge where point is
            if Assigned(NB) then
            begin
              // handle other triangle on other size of edge
              ThirdTriangle := SplitTriangle(NB, aPoint, NB.GetPOE(Triangle), NB2);
              // fix linking
              Triangle.Relink(NB, ThirdTriangle);
              NB.Relink(Triangle, SecondTriangle);
            end;
          end;
        end
        else
        begin // point is not in any triangle, so must be outside hull
          // find any starting point on hull
          Triangle := Triangles.FindFirstHullTriangle;
          // find any edge of triangle on hull
          poe := poeNoEdge;
          Triangle := Triangle.NextHullEdge(poe);
          // go CW to find first edge that can form triangle with point
          while not CanFormCWTriangle(Triangle, poe, aPoint)
          do Triangle := Triangle.NextHullEdge(poe);
          // form triangles prev
          PrevTriangle := Triangle;
          poePrev := poe;
          LastTriangle := nil;
          FirstTriangle := nil;
          repeat
            NewTriangle := CreateCWTriangle(PrevTriangle, poePrev, aPoint);
            if Assigned(LastTriangle) then
            begin
              NewTriangle.FNB1 := LastTriangle;
              LastTriangle.FNB3 := NewTriangle;
            end
            else FirstTriangle := NewTriangle;
            Triangles.add(NewTriangle);
            LastTriangle := NewTriangle;
            PrevTriangle := PrevTriangle.PrevHullEdge(poePrev);
          until (not Assigned(PrevTriangle)) or (not CanFormCWTriangle(PrevTriangle, poePrev, aPoint));
          // form triangles next
          NextTriangle := Triangle;
          poeNext := poe;
          LastTriangle := FirstTriangle;
          // first is already created so step to next
          NextTriangle := NextTriangle.NextHullEdge(poeNext);
          while Assigned(NextTriangle) and CanFormCWTriangle(NextTriangle, poeNext, aPoint) do
          begin
            NewTriangle := CreateCWTriangle(NextTriangle, poeNext, aPoint);
            NewTriangle.FNB3 := LastTriangle;
            LastTriangle.FNB1 := NewTriangle;
            Triangles.add(NewTriangle);
            LastTriangle := NewTriangle;
            NextTriangle := NextTriangle.NextHullEdge(poeNext);
          end;
        end;
      end
      else
      begin
        // no triangles so do completely new triangulation
        if not IsNaN(aMaxEdgeLengthX)
        then Triangulate(aMaxEdgeLengthX, aMaxEdgeLengthY, aNoData); // todo:
        // add triangles to intermediate grid (if exists)
      end;
    end;
  end
  else
  begin
    // point already in list -> ignore as new point but set new value and ID
    Points[i].ID := aPoint.ID;
    if not SameValue(Points[i].Value, aPoint.Value) then
    begin
      Points[i].Value := aPoint.Value;
      if Assigned(Triangles) then
      begin
        // mark connected triangles as NOT calculated
        AffectedTriangles := TDLTriangleList.Create;
        try
          Triangles.MarkAsUncalculated(Points[i], AffectedTriangles);
        finally
          AffectedTriangles.Free;
        end;
      end;
    end;
    aPoint.Free;
  end;
end;

function TDLNet.ChangePoint(aID: Integer; aValue: TDLValue): Boolean;
var
  i: Integer;
begin
  i := Points.IndexOf(aID);
  if i >= 0 then
  begin
    Points[i].Value := aValue;
    if Assigned(Triangles) then
    begin
      // mark connected triangles as NOT calculated
      Triangles.MarkAsUncalculated(Points[i]);
    end;
    Result := True;
  end
  else Result := False;
end;

procedure TDLNet.AddPoint(aPoint: TDLPoint; aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; var aExtent: TDLExtent; aNoData: TDLValue);

  function SplitTriangle(aTriangle: TDLTriangle; aPoint: TDLPoint; poe: Integer; out aNB: TDLTriangle): TDLTriangle;
  begin
    case poe of
      // split triangles with edges that contains this point and add 2 triangles for each
      poeEdge12:
        begin
          // create copy of triangle
          Result := TDLTriangle.Create;
          Result.Assign(aTriangle);
          // put point in both triangles on correct position
          aTriangle.FVertex2 := aPoint;
          Result.FVertex1 := aPoint;
          // fix neighbours
          if Assigned(aTriangle.FNB1)
          then aTriangle.FNB1.Relink(aTriangle, Result);
          aTriangle.FNB1 := Result;
          Result.FNB2 := aTriangle;
          // mark as not calculated
          aTriangle.NCalculated := False;
          Result.NCalculated := False;
          // add Result to all lists
          Triangles.add(Result);
          aExtent.ExtendExtent(Result);
          aNB := aTriangle.NB3;
        end;
      poeEdge23:
        begin
          Result := TDLTriangle.Create;
          Result.Assign(aTriangle);
          aTriangle.FVertex3 := aPoint;
          Result.FVertex2 := aPoint;
          if Assigned(aTriangle.FNB2)
          then aTriangle.FNB2.Relink(aTriangle, Result);
          aTriangle.FNB2 := Result;
          Result.FNB3 := aTriangle;
          aTriangle.NCalculated := False;
          Result.NCalculated := False;
          // add Result to all lists
          Triangles.add(Result);
          aExtent.ExtendExtent(Result);
          aNB := aTriangle.NB1;
        end;
      poeEdge31:
        begin
          Result := TDLTriangle.Create;
          Result.Assign(aTriangle);
          aTriangle.FVertex1 := aPoint;
          Result.FVertex3 := aPoint;
          if Assigned(aTriangle.FNB3)
          then aTriangle.FNB3.Relink(aTriangle, Result);
          aTriangle.FNB3 := Result;
          Result.FNB1 := aTriangle;
          aTriangle.NCalculated := False;
          Result.NCalculated := False;
          // add Result to all lists
          Triangles.add(Result);
          aExtent.ExtendExtent(Result);
          aNB := aTriangle.NB2;
        end;
    else
      Result := nil;
      aNB := nil;
    end;
  end;

var
  Triangle: TDLTriangle;
  nt: Integer;
  poe: Integer;
  NB: TDLTriangle;
  i: Integer;
  AffectedTriangles: TDLTriangleList;
  NewTriangle: TDLTriangle;
  NextTriangle: TDLTriangle;
  FirstTriangle: TDLTriangle;
  SecondTriangle: TDLTriangle;
  ThirdTriangle: TDLTriangle;
  NB2: TDLTriangle;
  PrevTriangle: TDLTriangle;
  poePrev: Integer;
  LastTriangle: TDLTriangle;
  poeNext: Integer;
begin
  i := Points.IndexOf(aPoint.x, aPoint.y);
  if i < 0 then
  begin
    // add point to owned points list
    Points.add(aPoint);
    if Assigned(Triangles) then
    begin
      if Triangles.Count > 0 then
      begin
        // todo: use cursor to search through triangles
        Triangle := Triangles.PointToTriangle(aPoint);
        if Assigned(Triangle) then
        begin
          // check if on a edge
          poe := Triangle.PointOnEdge(aPoint);
          if poe = poeNoEdge then
          begin
            // triangulate within triangle
            // create 3 new triangles that have new point as vertex2, NB2 points to edges of old triangle
            // edge12 of Triangle
            FirstTriangle := TDLTriangle.Create;
            FirstTriangle.FVertex1 := Triangle.Vertex2;
            FirstTriangle.FVertex2 := aPoint;
            FirstTriangle.FVertex3 := Triangle.Vertex1;
            // edge23 of Triangle
            SecondTriangle := TDLTriangle.Create;
            SecondTriangle.FVertex1 := Triangle.Vertex3;
            SecondTriangle.FVertex2 := aPoint;
            SecondTriangle.FVertex3 := Triangle.Vertex2;
            // edge31 of Triangle
            ThirdTriangle := TDLTriangle.Create;
            ThirdTriangle.FVertex1 := Triangle.Vertex1;
            ThirdTriangle.FVertex2 := aPoint;
            ThirdTriangle.FVertex3 := Triangle.Vertex3;
            // link missed NBs
            FirstTriangle.FNB1 := ThirdTriangle;
            FirstTriangle.FNB2 := Triangle.NB3;
            if Assigned(FirstTriangle.FNB2)
            then FirstTriangle.FNB2.Relink(Triangle, FirstTriangle);
            FirstTriangle.FNB3 := SecondTriangle;
            SecondTriangle.FNB1 := FirstTriangle;
            SecondTriangle.FNB2 := Triangle.NB1;
            if Assigned(SecondTriangle.FNB2)
            then SecondTriangle.FNB2.Relink(Triangle, SecondTriangle);
            SecondTriangle.FNB3 := ThirdTriangle;
            ThirdTriangle.FNB1 := SecondTriangle;
            ThirdTriangle.FNB2 := Triangle.NB2;
            if Assigned(ThirdTriangle.FNB2) then
              ThirdTriangle.FNB2.Relink(Triangle, ThirdTriangle);
            ThirdTriangle.FNB3 := FirstTriangle;
            // add new triangles to global list
            Triangles.add(FirstTriangle);
            Triangles.add(SecondTriangle);
            Triangles.add(ThirdTriangle);
            // all is linked now, remove old triangle
            Triangle.FNB1 := nil;
            Triangle.FNB2 := nil;
            Triangle.FNB3 := nil;
            aExtent.ExtendExtent(Triangle);
            Triangles.QuickRemove(Triangle);
          end
          else
          begin
            aExtent.ExtendExtent(Triangle);
            // split triangles with edges that contains this point and add 2 triangles for each
            SecondTriangle := SplitTriangle(Triangle, aPoint, poe, NB);
            // check for oposite triangle on edge where point is
            if Assigned(NB) then
            begin
              aExtent.ExtendExtent(NB);
              // handle other triangle on other size of edge
              ThirdTriangle := SplitTriangle(NB, aPoint, NB.GetPOE(Triangle), NB2);
              // fix linking
              Triangle.Relink(NB, ThirdTriangle);
              NB.Relink(Triangle, SecondTriangle);
            end;
          end;
        end
        else
        begin // point is not in any triangle, so must be outside hull
          // find any starting point on hull
          Triangle := Triangles.FindFirstHullTriangle;
          // find any edge of triangle on hull
          poe := poeNoEdge;
          Triangle := Triangle.NextHullEdge(poe);
          // go CW to find first edge that can form triangle with point
          while not CanFormCWTriangle(Triangle, poe, aPoint)
          do Triangle := Triangle.NextHullEdge(poe);
          // form triangles prev
          PrevTriangle := Triangle;
          poePrev := poe;
          LastTriangle := nil;
          FirstTriangle := nil;
          repeat
            NewTriangle := CreateCWTriangle(PrevTriangle, poePrev, aPoint);
            if Assigned(LastTriangle) then
            begin
              NewTriangle.FNB1 := LastTriangle;
              LastTriangle.FNB3 := NewTriangle;
            end
            else FirstTriangle := NewTriangle;
            Triangles.add(NewTriangle);
            aExtent.ExtendExtent(NewTriangle);
            LastTriangle := NewTriangle;
            PrevTriangle := PrevTriangle.PrevHullEdge(poePrev);
          until (not Assigned(PrevTriangle)) or (not CanFormCWTriangle(PrevTriangle, poePrev, aPoint));
          // form triangles next
          NextTriangle := Triangle;
          poeNext := poe;
          LastTriangle := FirstTriangle;
          // first is already created so step to next
          NextTriangle := NextTriangle.NextHullEdge(poeNext);
          while Assigned(NextTriangle) and CanFormCWTriangle(NextTriangle, poeNext, aPoint) do
          begin
            NewTriangle := CreateCWTriangle(NextTriangle, poeNext, aPoint);
            NewTriangle.FNB3 := LastTriangle;
            LastTriangle.FNB1 := NewTriangle;
            Triangles.add(NewTriangle);
            aExtent.ExtendExtent(NewTriangle);
            LastTriangle := NewTriangle;
            NextTriangle := NextTriangle.NextHullEdge(poeNext);
          end;
        end;
      end
      else
      begin
        // no triangles so do completely new triangulation
        if not IsNaN(aMaxEdgeLengthX)
        then Triangulate(aMaxEdgeLengthX, aMaxEdgeLengthY, aNoData); // todo:
        // return whole extent (over all points)
        aExtent := PointsToExtent;
      end;
    end;
  end
  else
  begin
    // point already in list -> ignore as new point but set new value and ID
    Points[i].ID := aPoint.ID;
    if not SameValue(Points[i].Value, aPoint.Value) then
    begin
      Points[i].Value := aPoint.Value;
      if Assigned(Triangles) then
      begin
        // mark connected triangles as NOT calculated
        AffectedTriangles := TDLTriangleList.Create;
        try
          Triangles.MarkAsUncalculated(Points[i], AffectedTriangles);
          for nt := 0 to AffectedTriangles.Count - 1
          do aExtent.ExtendExtent(AffectedTriangles[nt]);
        finally
          AffectedTriangles.Free;
        end;
      end;
    end;
    aPoint.Free;
  end;
end;

function TDLNet.ChangePoint(aID: Integer; aValue: TDLValue; var aExtent: TDLExtent): Boolean;
var
  i: Integer;
  AffectedTriangles: TDLTriangleList;
  t: Integer;
begin
  i := Points.IndexOf(aID);
  if i >= 0 then
  begin
    Points[i].Value := aValue;
    if Assigned(Triangles) then
    begin
      // mark connected triangles as NOT calculated
      AffectedTriangles := TDLTriangleList.Create;
      try
        Triangles.MarkAsUncalculated(Points[i], AffectedTriangles);
        for t := 0 to AffectedTriangles.Count - 1
        do aExtent.ExtendExtent(AffectedTriangles[t]);
      finally
        AffectedTriangles.Free;
      end;
    end;
    Result := True;
  end
  else Result := False;
end;
}
procedure TDLNet.Clear;
begin
  FTriangles.Clear;
  FPoints.Clear;
end;

constructor TDLNet.Create;
begin
  inherited Create;
  FPoints := TObjectDictionary<TDLID, TDLPoint>.Create([doOwnsValues]);
  FTriangles := TDLTriangleList.Create(True); // owns
end;

destructor TDLNet.Destroy;
begin
  FreeAndNil(FTriangles);
  FreeAndNil(FPoints);
  inherited;
end;

function TDLNet.PointsToExtent: TDLExtent;
var
  ipp: TPair<TDLID, TDLPoint>;
begin
  Result := TDLExtent.NoWorld;
  for ipp in points
  do Result.ExtendExtent(ipp.Value);
end;
{
function TDLNet.PointToTriangle(aPoint: TDLPoint): TDLTriangle;
begin
  Result := Triangles.PointToTriangle(aPoint);
end;

function TDLNet.PointToValue(aPoint: TDLPoint; aNoValue: TDLValue): TDLValue;
begin
  Result := Triangles.PointToValue(aPoint, aNoValue);
end;
}
procedure BuildHullGhostTriangles(aCenter: TDLPoint; aStartingTriangle: TDLTriangle;
  aHullTriangles, aInnerTriangles: TDLTriangleList; out aClosedHull: Boolean);
var
  CurrentTriangle: TDLTriangle;
  HullGhostTriangle: TDLTriangle;
begin
  CurrentTriangle := nil; // sentinel
  aClosedHull := True;
  while (aStartingTriangle <> CurrentTriangle) and aClosedHull do
  begin
    if not Assigned(CurrentTriangle) then
      CurrentTriangle := aStartingTriangle;
    // hull triangles are made of ghost triangles to be removed later
    // build ghost for current triangle: vertex2 is outside (and nil), nb2 points to outside triangle
    HullGhostTriangle := TDLTriangle.Create;
    if aCenter = CurrentTriangle.Vertex1 then
    begin
      HullGhostTriangle.FVertex1 := CurrentTriangle.Vertex2;
      HullGhostTriangle.FVertex3 := CurrentTriangle.Vertex3;
      HullGhostTriangle.FNB2 := CurrentTriangle.NB1;
    end
    else
    begin
      if aCenter = CurrentTriangle.Vertex2 then
      begin
        HullGhostTriangle.FVertex1 := CurrentTriangle.Vertex3;
        HullGhostTriangle.FVertex3 := CurrentTriangle.Vertex1;
        HullGhostTriangle.FNB2 := CurrentTriangle.NB2;
      end
      else
      begin
        HullGhostTriangle.FVertex1 := CurrentTriangle.Vertex1;
        HullGhostTriangle.FVertex3 := CurrentTriangle.Vertex2;
        HullGhostTriangle.FNB2 := CurrentTriangle.NB3;
      end;
    end;
    aHullTriangles.add(HullGhostTriangle);
    aInnerTriangles.add(CurrentTriangle);
    CurrentTriangle := CurrentTriangle.NextLeftTriangle(aCenter);
    if not Assigned(CurrentTriangle) then
    begin
      // hull around center is not closed, aCenter must be on overall hull
      // we are at the end at this direction
      // now find end in other direction and insert hull triangles before start
      CurrentTriangle := aStartingTriangle.NextRightTriangle(aCenter);
      while Assigned(CurrentTriangle) do
      begin
        HullGhostTriangle := TDLTriangle.Create;
        if aCenter = CurrentTriangle.Vertex1 then
        begin
          HullGhostTriangle.FVertex1 := CurrentTriangle.Vertex2;
          HullGhostTriangle.FVertex3 := CurrentTriangle.Vertex3;
          HullGhostTriangle.FNB2 := CurrentTriangle.NB1;
        end
        else
        begin
          if aCenter = CurrentTriangle.Vertex2 then
          begin
            HullGhostTriangle.FVertex1 := CurrentTriangle.Vertex3;
            HullGhostTriangle.FVertex3 := CurrentTriangle.Vertex1;
            HullGhostTriangle.FNB2 := CurrentTriangle.NB2;
          end
          else
          begin
            HullGhostTriangle.FVertex1 := CurrentTriangle.Vertex1;
            HullGhostTriangle.FVertex3 := CurrentTriangle.Vertex2;
            HullGhostTriangle.FNB2 := CurrentTriangle.NB3;
          end;
        end;
        aHullTriangles.Insert(0, HullGhostTriangle);
        aInnerTriangles.add(CurrentTriangle);
        CurrentTriangle := CurrentTriangle.NextRightTriangle(aCenter);
      end;
      aClosedHull := False;
    end;
  end;
end;

function CreateTriangleOnLocalHull(aHullTriangleA, aHullTriangleB: TDLTriangle;
  aHullTrianglesToCheck: TDLTriangleList): TDLTriangle;
var
  AllPointsOutside: Boolean;
  t: Integer;
begin
  if Side(aHullTriangleA.Vertex1, aHullTriangleA.Vertex3, aHullTriangleB.Vertex1) = sideRight then
  begin
    AllPointsOutside := True;
    if Assigned(aHullTrianglesToCheck) then
    begin
      for t := 0 to aHullTrianglesToCheck.Count - 1 do
      begin
        if (aHullTrianglesToCheck[t] <> aHullTriangleA) and (aHullTrianglesToCheck[t] <> aHullTriangleB) and
          (aHullTrianglesToCheck[t].Vertex1 <> aHullTriangleA.Vertex3) and (Side(aHullTriangleA.Vertex3,
            aHullTriangleB.Vertex1, aHullTrianglesToCheck[t].Vertex1) <> sideLeft) and
          (Side(aHullTriangleB.Vertex1, aHullTriangleB.Vertex3, aHullTrianglesToCheck[t].Vertex1) <> sideLeft) and
          (Side(aHullTriangleA.Vertex1, aHullTriangleA.Vertex3, aHullTrianglesToCheck[t].Vertex1) <> sideLeft) then
        begin
          AllPointsOutside := False;
          Break;
        end;
      end;
    end;
    if AllPointsOutside then
    begin
      Result := TDLTriangle.Create;
      Result.FVertex1 := aHullTriangleB.Vertex1;
      Result.FVertex2 := aHullTriangleA.Vertex1;
      Result.FVertex3 := aHullTriangleA.Vertex3;
      if aHullTriangleA.IsGhost then
      begin
        Result.FNB1 := aHullTriangleA.NB2;
        aHullTriangleA.Free;
      end
      else Result.FNB1 := aHullTriangleA;
      if Assigned(Result.FNB1)
      then Result.FNB1.LinkTo(Result, Result.Vertex3, Result.Vertex2);
      if aHullTriangleB.IsGhost then
      begin
        Result.FNB3 := aHullTriangleB.NB2;
        aHullTriangleB.Free;
      end
      else
        Result.FNB3 := aHullTriangleB;
      if Assigned(Result.FNB3)
      then Result.FNB3.LinkTo(Result, Result.Vertex2, Result.Vertex1);
    end
    else Result := nil;
  end
  else Result := nil;
end;

procedure TriangulateClosedHull(aHullTriangles, aTrianglesOwner: TDLTriangleList); overload;
var
  NewTriangle: TDLTriangle;
  t: Integer;
  T2: Integer;
begin
  // fill hull with triangles by going around the hull and filling the inside like a spider
  t := 0;
  while aHullTriangles.Count > 3 do
  begin
    if t >= aHullTriangles.Count
    then t := 0;
    T2 := t + 1;
    if T2 >= aHullTriangles.Count
    then T2 := 0;
    NewTriangle := CreateTriangleOnLocalHull(aHullTriangles[t], aHullTriangles[T2], aHullTriangles);
    if Assigned(NewTriangle) then
    begin
      // put new triangle in place t and delete t2
      aHullTriangles.Items[t] := NewTriangle;
      aHullTriangles.Delete(T2);
      aTrianglesOwner.add(NewTriangle);
      // do not use same position because of fanning triangles (so use next)
    end;
    t := T2;
  end;
  // do the last 3 hull triangles and fill with exactly 1 triangle; remove ghosts if they exist
  NewTriangle := TDLTriangle.Create;
  NewTriangle.FVertex1 := aHullTriangles[1].Vertex1;
  NewTriangle.FVertex2 := aHullTriangles[0].Vertex1;
  NewTriangle.FVertex3 := aHullTriangles[2].Vertex1;
  if aHullTriangles[0].IsGhost then
  begin
    NewTriangle.FNB1 := aHullTriangles[0].NB2;
    aHullTriangles[0].Free;
  end
  else NewTriangle.FNB1 := aHullTriangles[0];
  if Assigned(NewTriangle.FNB1)
  then NewTriangle.FNB1.LinkTo(NewTriangle, NewTriangle.Vertex3, NewTriangle.Vertex2);
  if aHullTriangles[1].IsGhost then
  begin
    NewTriangle.FNB3 := aHullTriangles[1].NB2;
    aHullTriangles[1].Free;
  end
  else NewTriangle.FNB3 := aHullTriangles[1];
  if Assigned(NewTriangle.FNB3)
  then NewTriangle.FNB3.LinkTo(NewTriangle, NewTriangle.Vertex2, NewTriangle.Vertex1);
  if aHullTriangles[2].IsGhost then
  begin
    NewTriangle.FNB2 := aHullTriangles[2].NB2;
    aHullTriangles[2].Free;
  end
  else NewTriangle.FNB2 := aHullTriangles[2];
  if Assigned(NewTriangle.FNB2)
  then NewTriangle.FNB2.LinkTo(NewTriangle, NewTriangle.Vertex1, NewTriangle.Vertex3);
  aTrianglesOwner.add(NewTriangle);
  // all ghosts are freed, rest is already in aTrianglesOwner
end;

procedure TriangulateClosedHull(aHullTriangles, aTrianglesOwner: TDLTriangleList; var aExtent: TDLExtent); overload;
var
  NewTriangle: TDLTriangle;
  t: Integer;
  T2: Integer;
begin
  // fill hull with triangles by going around the hull and filling the inside like a spider
  t := 0;
  while aHullTriangles.Count > 3 do
  begin
    if t >= aHullTriangles.Count
    then t := 0;
    T2 := t + 1;
    if T2 >= aHullTriangles.Count
    then T2 := 0;
    NewTriangle := CreateTriangleOnLocalHull(aHullTriangles[t], aHullTriangles[T2], aHullTriangles);
    if Assigned(NewTriangle) then
    begin
      // put new triangle in place t and delete t2
      aHullTriangles.Items[t] := NewTriangle;
      aHullTriangles.Delete(T2);
      aTrianglesOwner.add(NewTriangle);
      aExtent.ExtendExtent(NewTriangle);
      // do not use same position because of fanning triangles (so use next)
    end;
    t := T2;
  end;
  // do the last 3 hull triangles and fill with exactly 1 triangle; remove ghosts if they exist
  NewTriangle := TDLTriangle.Create;
  NewTriangle.FVertex1 := aHullTriangles[1].Vertex1;
  NewTriangle.FVertex2 := aHullTriangles[0].Vertex1;
  NewTriangle.FVertex3 := aHullTriangles[2].Vertex1;
  if aHullTriangles[0].IsGhost then
  begin
    NewTriangle.FNB1 := aHullTriangles[0].NB2;
    aHullTriangles[0].Free;
  end
  else NewTriangle.FNB1 := aHullTriangles[0];
  if Assigned(NewTriangle.FNB1)
  then NewTriangle.FNB1.LinkTo(NewTriangle, NewTriangle.Vertex3, NewTriangle.Vertex2);
  if aHullTriangles[1].IsGhost then
  begin
    NewTriangle.FNB3 := aHullTriangles[1].NB2;
    aHullTriangles[1].Free;
  end
  else NewTriangle.FNB3 := aHullTriangles[1];
  if Assigned(NewTriangle.FNB3)
  then NewTriangle.FNB3.LinkTo(NewTriangle, NewTriangle.Vertex2, NewTriangle.Vertex1);
  if aHullTriangles[2].IsGhost then
  begin
    NewTriangle.FNB2 := aHullTriangles[2].NB2;
    aHullTriangles[2].Free;
  end
  else NewTriangle.FNB2 := aHullTriangles[2];
  if Assigned(NewTriangle.FNB2)
  then NewTriangle.FNB2.LinkTo(NewTriangle, NewTriangle.Vertex1, NewTriangle.Vertex3);
  aTrianglesOwner.add(NewTriangle);
  aExtent.ExtendExtent(NewTriangle);
  // all ghosts are freed, rest is already in aTrianglesOwner
end;

procedure TriangulateOpenHull(aHullTriangles, aTrianglesOwner: TDLTriangleList); overload;
var
  t: Integer;
  TriangleProcessed: Boolean;
  NewTriangle: TDLTriangle;
begin
  TriangleProcessed := True; // sentinel
  while (aHullTriangles.Count > 1) and TriangleProcessed do
  begin
    TriangleProcessed := False;
    // find max hull from starting point by checking order
    t := 0;
    while t < aHullTriangles.Count - 1 do
    begin
      NewTriangle := CreateTriangleOnLocalHull(aHullTriangles[t], aHullTriangles[t + 1], aHullTriangles);
      if Assigned(NewTriangle) then
      begin
        aHullTriangles.Items[t] := NewTriangle;
        aHullTriangles.Delete(t + 1);
        aTrianglesOwner.add(NewTriangle);
        // do not use same position because of fanning triangles (so use next)
        TriangleProcessed := True;
      end;
      t := t + 1;
    end;
  end;
  // remove ghosts that are left in hull
  // these are not linked to any real triangles inside the new local hull
  // but can reference a triangle outside this local hull that is now on the hull itself (nb is already nil)
  for t := aHullTriangles.Count - 1 downto 0 do
  begin
    if aHullTriangles[t].IsGhost
    then aHullTriangles[t].Free;
  end;
end;

procedure TriangulateOpenHull(aHullTriangles, aTrianglesOwner: TDLTriangleList; var aExtent: TDLExtent); overload;
var
  t: Integer;
  TriangleProcessed: Boolean;
  NewTriangle: TDLTriangle;
begin
  TriangleProcessed := True; // sentinel
  while (aHullTriangles.Count > 1) and TriangleProcessed do
  begin
    TriangleProcessed := False;
    // find max hull from starting point by checking order
    t := 0;
    while t < aHullTriangles.Count - 1 do
    begin
      NewTriangle := CreateTriangleOnLocalHull(aHullTriangles[t], aHullTriangles[t + 1], aHullTriangles);
      if Assigned(NewTriangle) then
      begin
        aHullTriangles.Items[t] := NewTriangle;
        aHullTriangles.Delete(t + 1);
        aTrianglesOwner.add(NewTriangle);
        aExtent.ExtendExtent(NewTriangle);
        // do not use same position because of fanning triangles (so use next)
        TriangleProcessed := True;
      end;
      t := t + 1;
    end;
  end;
  // remove ghosts that are left in hull
  // these are not linked to any real triangles inside the new local hull
  // but can reference a triangle outside this local hull that is now on the hull itself (nb is already nil)
  for t := aHullTriangles.Count - 1 downto 0 do
  begin
    if aHullTriangles[t].IsGhost
    then aHullTriangles[t].Free;
  end;
end;

{
function TDLNet.RemovePoint(aID: Integer): Boolean;
var
  p: Integer;
  InnerTriangles: TDLTriangleList;
  Point: TDLPoint;
  t: Integer;
  HullTriangles: TDLTriangleList;
  ClosedHull: Boolean;
begin
  // remove point (and connected triangles) and triangulate new empty space
  p := Points.IndexOf(aID);
  if p >= 0 then
  begin
    if Assigned(Triangles) then
    begin
      // check if there are any triangles
      if Triangles.Count > 0 then
      begin
        if Points.Count > 3 then
        begin
          Point := Points[p];
          Triangles.FindFirstTriangle(Point);
          HullTriangles := TDLTriangleList.Create;
          InnerTriangles := TDLTriangleList.Create;
          try
            BuildHullGhostTriangles(Point, Triangles.FindFirstTriangle(Point), HullTriangles, InnerTriangles, ClosedHull);
            for t := 0 to InnerTriangles.Count - 1 do
            begin
              InnerTriangles[t].Unlink;
              Triangles.QuickRemove(InnerTriangles[t]);
            end;
            Points.QuickDelete(p);
            if ClosedHull
            then TriangulateClosedHull(HullTriangles, Triangles)
            else TriangulateOpenHull(HullTriangles, Triangles);
          finally
            HullTriangles.Free;
            InnerTriangles.Free;
            FOrphanes := aID;
          end;
        end
        else
        begin // less then 3 points, no triangles possible
          Triangles.Clear;
          Points.Delete(p); // short list, no need for quickdelete
        end;
      end
      else
      begin
        // no triangles so just remove seperate points
        Points.QuickDelete(p);
      end;
    end
    else
    begin
      // no triangles so just remove seperate points
      Points.QuickDelete(p);
    end;
    Result := True;
  end
  else Result := False;
end;

function TDLNet.RemovePoint(aID: Integer; var aExtent: TDLExtent): Boolean;
var
  p: Integer;
  InnerTriangles: TDLTriangleList;
  Point: TDLPoint;
  t: Integer;
  HullTriangles: TDLTriangleList;
  ClosedHull: Boolean;
begin
  // remove point (and connected triangles) and triangulate new empty space
  p := Points.IndexOf(aID);
  if p >= 0 then
  begin
    if Assigned(Triangles) then
    begin
      // check if there are any triangles
      if Triangles.Count > 0 then
      begin
        if Points.Count > 3 then
        begin
          Point := Points[p];
          Triangles.FindFirstTriangle(Point);
          HullTriangles := TDLTriangleList.Create;
          InnerTriangles := TDLTriangleList.Create;
          try
            BuildHullGhostTriangles(Point, Triangles.FindFirstTriangle(Point), HullTriangles, InnerTriangles, ClosedHull);
            for t := 0 to InnerTriangles.Count - 1 do
            begin
              InnerTriangles[t].Unlink;
              aExtent.ExtendExtent(InnerTriangles[t]);
              Triangles.QuickRemove(InnerTriangles[t]);
            end;
            Points.QuickDelete(p);
            if ClosedHull
            then TriangulateClosedHull(HullTriangles, Triangles, aExtent)
            else TriangulateOpenHull(HullTriangles, Triangles, aExtent);
          finally
            HullTriangles.Free;
            InnerTriangles.Free;
            FOrphanes := aID;
          end;
        end
        else
        begin // less then 3 points, no triangles possible
          aExtent.ExtendExtent(Points[p]);
          Triangles.Clear;
          Points.Delete(p); // short list, no need for quickdelete
        end;
      end
      else
      begin
        // no triangles so just remove seperate points
        aExtent.ExtendExtent(Points[p]);
        Points.QuickDelete(p);
      end;
    end
    else
    begin
      // no triangles so just remove seperate points
      aExtent.ExtendExtent(Points[p]);
      Points.QuickDelete(p);
    end;
    Result := True;
  end
  else Result := False;
end;
}
function TDLNet.RemoveDuplicates: Boolean;
var
  _points: TDLPointList;
  i: Integer;
  ipp: TPair<TDLID, TDLPoint>;
begin
  _points := TDLPointList.Create;
  try
    // step 1: sort list
    for ipp in points
    do _points.Add(ipp.Value);
    _points.SortXY;
    // step 2: remove duplicates
    Result := False;
    for i := 0 to _points.Count - 2 do
    begin
      if IsEqualXY(_points[i], _points[i + 1]) then
      begin
        // find and remove point
        for ipp in points do
        begin
          if ipp.Value=_points[i] then
          begin
            points.Remove(ipp.Key);
            break;
          end;
        end;
        Result := True;
      end;
    end;
  finally
    _points.Free;
  end;
end;

procedure TDLNet.SaveToFile(const aFileName: string);
var
  F: TextFile;
  t: Integer;
  FormatSettings: TFormatSettings;
begin
  FillChar(FormatSettings, SizeOf(FormatSettings), 0);
  FormatSettings.DecimalSeparator := '.';
  AssignFile(F, aFileName);
  Rewrite(F);
  try
    WriteLn(F, 'id'
      +ccTab+'x1'+ccTab+'y1'+ccTab+'v1'
      +ccTab+'x2'+ccTab+'y2'+ccTab+'v2'
      +ccTab+'x3'+ccTab+'y3'+ccTab+'v3');
    for t := 0 to FTriangles.Count - 1 do
    begin
      WriteLn(F, IntToStr(t)
                 +ccTab+FloatToStr(FTriangles[t].FVertex1.x, FormatSettings)+ccTab+FloatToStr(FTriangles[t].FVertex1.y, FormatSettings)+ccTab+FloatToStr(FTriangles[t].FVertex1.value, FormatSettings)
                 +ccTab+FloatToStr(FTriangles[t].FVertex2.x, FormatSettings)+ccTab+FloatToStr(FTriangles[t].FVertex2.y, FormatSettings)+ccTab+FloatToStr(FTriangles[t].FVertex2.value, FormatSettings)
                 +ccTab+FloatToStr(FTriangles[t].FVertex3.x, FormatSettings)+ccTab+FloatToStr(FTriangles[t].FVertex3.y, FormatSettings)+ccTab+FloatToStr(FTriangles[t].FVertex3.value, FormatSettings));
    end;
  finally
    CloseFile(F);
  end;
end;
(*
procedure TDLNet.setPoints(aPoints: TDLPointList);
begin
  FreeAndNil(FTriangles);
  FPoints.Free;
  FPoints := aPoints;
end;
*)
procedure TDLNet.Triangulate(aMaxEdgeLengthX, aMaxEdgeLengthY: TDLCoordinate; aNodata: TDLValue; aThreadPool: TMyThreadPool;
  aCheckForDuplicates: Boolean; aParam: Integer);
var
  TopNode: TDLNode;
  ipp: TPair<TDLID, TDLPoint>;
  {$IFDEF PERFORMANCELOG}
  StartTime: Int64;
  {$ENDIF}
begin
  {$IFDEF PERFORMANCELOG}
  StartTime := HighResTick;
  {$ENDIF}
  if aCheckForDuplicates
  then RemoveDuplicates;
  {$IFDEF PERFORMANCELOG}
  Log.WriteLn(    'RemoveDuplicates:    '+HighResTickDurationStr(StartTime), llNormal, 2);
  StartTime := HighResTick;
  {$ENDIF}
  if FPoints.Count > 2 then
  begin
    TopNode := TDLNode.Create(nil);
    try
      // put all points in top node, unsorted
      for ipp in points
      do TopNode.Points.Add(ipp.value);
      //TopNode.Points.FSortOrder := FPoints.SortOrder;
      //TopNode.Points.SortXY(aThreadPool);
      {$IFDEF PERFORMANCELOG}
      Log.WriteLn('Assign:                '+HighResTickDurationStr(StartTime), llNormal, 2);
      StartTime := HighResTick;
      {$ENDIF}
      // create triangles
      TopNode.DevidePoints(aThreadPool, aParam);
      {$IFDEF PERFORMANCELOG}
      Log.WriteLn('DevidePoints:          '+HighResTickDurationStr(StartTime), llNormal, 2);
      StartTime := HighResTick;
      {$ENDIF}
      TopNode.Triangulate(aThreadPool, aParam);
      {$IFDEF PERFORMANCELOG}
      Log.WriteLn('Triangulate:           '+HighResTickDurationStr(StartTime), llNormal, 2);
      StartTime := HighResTick;
      {$ENDIF}
      TopNode.Merge(aThreadPool, aParam);
      {$IFDEF PERFORMANCELOG}
      Log.WriteLn('Merge:                 '+HighResTickDurationStr(StartTime), llNormal, 2);
      StartTime := HighResTick;
      {$ENDIF}
      TopNode.RemoveGhostsTriangles;
      {$IFDEF PERFORMANCELOG}
      Log.WriteLn('RemoveGhostsTriangles: '+HighResTickDurationStr(StartTime), llNormal, 2);
      StartTime := HighResTick;
      {$ENDIF}
      // move found triangles to current triangles
      Triangles.Assign(TopNode.Triangles);
      {$IFDEF PERFORMANCELOG}
      Log.WriteLn('Assign:                '+HighResTickDurationStr(StartTime), llNormal, 2);
      {$ENDIF}
      if not IsNaN(aMaxEdgeLengthX)
      then Triangles.DisableTooLongEdgesAndNoData(aMaxEdgeLengthX, aMaxEdgeLengthY, aNoData);
      {$IFDEF PERFORMANCELOG}
      Log.WriteLn('Disable:               '+HighResTickDurationStr(StartTime), llNormal, 2);
      {$ENDIF}
    finally
      TopNode.Free;
    end;
  end;
end;

function TDLNet.ValueAtPoint(var aCursor: TDLTriangle; x, y: TDLCoordinate; aNoValue: TDLValue): TDLValue;
// keep cursor on valid triangle and signal aNoDataValue when outside triangulation
var
  Found: Boolean;
  Triangle: TDLTriangle;
begin
  if Triangles.Count>0 then
  begin
    // if no cursor defined use first triangle as cursor
    if not Assigned(aCursor)
    then aCursor := Triangles[0];
    // try to find containing triangle
    Triangle := nil;
    Found := False;
    while not Found do
    begin
      // check if outside triangle at specific side: traverse in that direction
      if Side(aCursor.Vertex1, aCursor.Vertex2, x, y)>0 then
      begin
        if Assigned(aCursor.NB3)
        then aCursor := aCursor.NB3
        else
        begin // outside boundary
          Found := True;
          Triangle := nil;
        end;
      end
      else
      begin
        if Side(aCursor.Vertex2, aCursor.Vertex3, x, y)>0 then
        begin
          if Assigned(aCursor.NB1)
          then aCursor := aCursor.NB1
          else
          begin // outside boundary
            Found := True;
            Triangle := nil;
          end;
        end
        else
        begin
          if Side(aCursor.Vertex3, aCursor.Vertex1, x, y)>0 then
          begin
            if Assigned(aCursor.NB2)
            then aCursor := aCursor.NB2
            else
            begin // outside boundary
              Found := True;
              Triangle := nil;
            end;
          end
          else
          begin // inside this triangle
            Triangle := aCursor;
            Found := True;
          end;
        end;
      end;
    end;
    // get value within containing triangle
    if Assigned(Triangle) and not Triangle.Disabled
    then Result := Triangle.ValueOnPoint(x, y)
    else Result := aNoValue;
  end
  else Result := aNoValue;
end;

end.
