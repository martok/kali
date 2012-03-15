unit uEntities;

interface

uses uNetworking;

type
  TEntity = class
  private
    FID: TEntityID;
  protected
  public
    constructor Create(ID: TEntityID);
    destructor Destroy; override;
    property ID: TEntityID read FID;

    procedure EntityMessage(Connection: TConnectionState; Method: Word; Params: array of Variant); virtual;
  end;

implementation

{ TEntity }

constructor TEntity.Create(ID: TEntityID);
begin
  inherited Create;
  FID:= ID;
end;

destructor TEntity.Destroy;
begin
  inherited;
end;

procedure TEntity.EntityMessage(Connection: TConnectionState; Method: Word; Params: array of Variant);
begin
end;

end.
