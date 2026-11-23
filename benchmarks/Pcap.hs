{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Self-contained PCAP (Ethernet II + IPv4/IPv6 + TCP/UDP/DNS) decoder and
-- encoder built on "Data.Binary.Get" and "Data.Binary.Put".
--
-- Complements "Wasm" by exercising consecutive fixed-size multi-byte reads
-- (big-endian and little-endian Word16/Word32/Word64), applicative header
-- parsers (for @readN/readN merge@), @isolate@ per packet, and @Generic@
-- @Binary@ on record-heavy types.
module Pcap
  ( PcapFile(..)
  , PcapGlobalHeader(..)
  , PacketRecord(..)
  , PacketHeader(..)
  , EthFrame(..)
  , MacAddr(..)
  , NetworkPayload(..)
  , Ipv4Header(..)
  , Ipv6Header(..)
  , TransportPayload(..)
  , TcpHeader(..)
  , UdpHeader(..)
  , DnsMessage(..)
  , DnsHeader(..)
  , getPcapFile
  , getPcapHeadersOnly
  , putPcapFile
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Binary (Binary)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import Data.Int
import Data.Word
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- PCAP / Network AST

data PcapFile = PcapFile
  { pcapHeader  :: !PcapGlobalHeader
  , pcapPackets :: ![PacketRecord]
  } deriving (Eq, Show, Generic)

data PcapGlobalHeader = PcapGlobalHeader
  { ghVersionMajor :: !Word16
  , ghVersionMinor :: !Word16
  , ghThisZone     :: !Int32
  , ghSigFigs      :: !Word32
  , ghSnapLen      :: !Word32
  , ghNetwork      :: !Word32
  } deriving (Eq, Show, Generic)

data PacketRecord = PacketRecord
  { prHeader :: !PacketHeader
  , prFrame  :: !EthFrame
  } deriving (Eq, Show, Generic)

data PacketHeader = PacketHeader
  { phTsSec   :: !Word32
  , phTsUsec  :: !Word32
  , phInclLen :: !Word32
  , phOrigLen :: !Word32
  } deriving (Eq, Show, Generic)

data MacAddr = MacAddr !Word16 !Word32
  deriving (Eq, Show, Generic)

data EthFrame = EthFrame
  { ethDst     :: !MacAddr
  , ethSrc     :: !MacAddr
  , ethType    :: !Word16
  , ethPayload :: !NetworkPayload
  } deriving (Eq, Show, Generic)

data NetworkPayload
  = NetIpv4 !Ipv4Header !B.ByteString !TransportPayload
  | NetIpv6 !Ipv6Header !TransportPayload
  | NetOther !B.ByteString
  deriving (Eq, Show, Generic)

data Ipv4Header = Ipv4Header
  { ip4VerIhl    :: !Word8
  , ip4DscpEcn   :: !Word8
  , ip4TotalLen  :: !Word16
  , ip4Ident     :: !Word16
  , ip4FlagsFrag :: !Word16
  , ip4Ttl       :: !Word8
  , ip4Proto     :: !Word8
  , ip4Checksum  :: !Word16
  , ip4Src       :: !Word32
  , ip4Dst       :: !Word32
  } deriving (Eq, Show, Generic)

data Ipv6Header = Ipv6Header
  { ip6VerTcFl   :: !Word32
  , ip6PayloadLn :: !Word16
  , ip6NextHdr   :: !Word8
  , ip6HopLimit  :: !Word8
  , ip6SrcHi     :: !Word64
  , ip6SrcLo     :: !Word64
  , ip6DstHi     :: !Word64
  , ip6DstLo     :: !Word64
  } deriving (Eq, Show, Generic)

data TransportPayload
  = TransTcp !TcpHeader !B.ByteString !B.ByteString
  | TransUdp !UdpHeader !(Maybe DnsMessage) !B.ByteString
  | TransOther !B.ByteString
  deriving (Eq, Show, Generic)

data TcpHeader = TcpHeader
  { tcpSrcPort    :: !Word16
  , tcpDstPort    :: !Word16
  , tcpSeqNum     :: !Word32
  , tcpAckNum     :: !Word32
  , tcpOffFlags   :: !Word16
  , tcpWindow     :: !Word16
  , tcpChecksum   :: !Word16
  , tcpUrgentPtr  :: !Word16
  } deriving (Eq, Show, Generic)

data UdpHeader = UdpHeader
  { udpSrcPort  :: !Word16
  , udpDstPort  :: !Word16
  , udpLen      :: !Word16
  , udpChecksum :: !Word16
  } deriving (Eq, Show, Generic)

data DnsMessage = DnsMessage
  { dnsHeader :: !DnsHeader
  , dnsBody   :: !B.ByteString
  } deriving (Eq, Show, Generic)

data DnsHeader = DnsHeader
  { dnsTxId    :: !Word16
  , dnsFlags   :: !Word16
  , dnsQdCount :: !Word16
  , dnsAnCount :: !Word16
  , dnsNsCount :: !Word16
  , dnsArCount :: !Word16
  } deriving (Eq, Show, Generic)

instance NFData PcapFile
instance NFData PcapGlobalHeader
instance NFData PacketRecord
instance NFData PacketHeader
instance NFData MacAddr
instance NFData EthFrame
instance NFData NetworkPayload
instance NFData Ipv4Header
instance NFData Ipv6Header
instance NFData TransportPayload
instance NFData TcpHeader
instance NFData UdpHeader
instance NFData DnsMessage
instance NFData DnsHeader

instance Binary PcapFile
instance Binary PcapGlobalHeader
instance Binary PacketRecord
instance Binary PacketHeader
instance Binary MacAddr
instance Binary EthFrame
instance Binary NetworkPayload
instance Binary Ipv4Header
instance Binary Ipv6Header
instance Binary TransportPayload
instance Binary TcpHeader
instance Binary UdpHeader
instance Binary DnsMessage
instance Binary DnsHeader

------------------------------------------------------------------------
-- Decoder

getPcapFile :: Get PcapFile
getPcapFile = do
  gh <- getGlobalHeader
  pkts <- getPackets []
  return (PcapFile gh pkts)
  where
    getPackets acc = do
      done <- isEmpty
      if done
        then return $! reverse acc
        else do
          !pkt <- getPacketRecord
          getPackets (pkt : acc)

-- | Decode only PCAP global + per-packet headers, skipping packet payloads.
getPcapHeadersOnly :: Get [PacketHeader]
getPcapHeadersOnly = do
  _ <- getGlobalHeader
  go []
  where
    go acc = do
      done <- isEmpty
      if done
        then return $! reverse acc
        else do
          !hdr <- getPacketHeader
          skip (fromIntegral (phInclLen hdr))
          go (hdr : acc)

getGlobalHeader :: Get PcapGlobalHeader
getGlobalHeader = do
  magic <- getWord32le
  when (magic /= 0xa1b2c3d4) $
    fail ("Unsupported PCAP magic: " ++ show magic)
  PcapGlobalHeader
    <$> getWord16le
    <*> getWord16le
    <*> getInt32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

getPacketHeader :: Get PacketHeader
getPacketHeader =
  PacketHeader
    <$> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
{-# INLINE getPacketHeader #-}

getPacketRecord :: Get PacketRecord
getPacketRecord = do
  hdr <- getPacketHeader
  let !len = fromIntegral (phInclLen hdr)
  frame <- isolate len (getEthFrame len)
  return (PacketRecord hdr frame)

getMacAddr :: Get MacAddr
getMacAddr = MacAddr <$> getWord16be <*> getWord32be
{-# INLINE getMacAddr #-}

getEthFrame :: Int -> Get EthFrame
getEthFrame totalLen = do
  dst <- getMacAddr
  src <- getMacAddr
  et  <- getWord16be
  let !remLen = totalLen - 14
  payload <- case et of
    0x0800 -> getIpv4 remLen
    0x86dd -> getIpv6 remLen
    _      -> NetOther <$> getByteString remLen
  return (EthFrame dst src et payload)

getIpv4Header :: Get Ipv4Header
getIpv4Header =
  Ipv4Header
    <$> getWord8
    <*> getWord8
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord8
    <*> getWord8
    <*> getWord16be
    <*> getWord32be
    <*> getWord32be
{-# INLINE getIpv4Header #-}

getIpv4 :: Int -> Get NetworkPayload
getIpv4 remLen = do
  hdr <- getIpv4Header
  let !ihlBytes = fromIntegral (ip4VerIhl hdr .&. 0x0f) * 4
      !optLen   = max 0 (ihlBytes - 20)
  opts <- getByteString optLen
  let !bodyLen = remLen - 20 - optLen
  trans <- getTransport (ip4Proto hdr) bodyLen
  return (NetIpv4 hdr opts trans)

getIpv6Header :: Get Ipv6Header
getIpv6Header =
  Ipv6Header
    <$> getWord32be
    <*> getWord16be
    <*> getWord8
    <*> getWord8
    <*> getWord64be
    <*> getWord64be
    <*> getWord64be
    <*> getWord64be
{-# INLINE getIpv6Header #-}

getIpv6 :: Int -> Get NetworkPayload
getIpv6 remLen = do
  hdr <- getIpv6Header
  trans <- getTransport (ip6NextHdr hdr) (remLen - 40)
  return (NetIpv6 hdr trans)

getTransport :: Word8 -> Int -> Get TransportPayload
getTransport proto remLen = case proto of
  6  -> do
    hdr <- getTcpHeader
    let !dataOff = fromIntegral ((tcpOffFlags hdr `shiftR` 12) .&. 0x0f) * 4
        !optLen  = max 0 (dataOff - 20)
    opts <- getByteString optLen
    payload <- getByteString (remLen - 20 - optLen)
    return (TransTcp hdr opts payload)
  17 -> do
    hdr <- getUdpHeader
    let !bodyLen = remLen - 8
    if (udpSrcPort hdr == 53 || udpDstPort hdr == 53) && bodyLen >= 12
      then do
        dnsHdr <- getDnsHeader
        dnsRest <- getByteString (bodyLen - 12)
        return (TransUdp hdr (Just (DnsMessage dnsHdr dnsRest)) B.empty)
      else do
        payload <- getByteString bodyLen
        return (TransUdp hdr Nothing payload)
  _ -> TransOther <$> getByteString remLen

getTcpHeader :: Get TcpHeader
getTcpHeader =
  TcpHeader
    <$> getWord16be
    <*> getWord16be
    <*> getWord32be
    <*> getWord32be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
{-# INLINE getTcpHeader #-}

getUdpHeader :: Get UdpHeader
getUdpHeader =
  UdpHeader
    <$> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
{-# INLINE getUdpHeader #-}

getDnsHeader :: Get DnsHeader
getDnsHeader =
  DnsHeader
    <$> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
{-# INLINE getDnsHeader #-}

------------------------------------------------------------------------
-- Encoder

putPcapFile :: PcapFile -> Put
putPcapFile (PcapFile gh pkts) =
  putGlobalHeader gh <> foldMap putPacketRecord pkts

putGlobalHeader :: PcapGlobalHeader -> Put
putGlobalHeader (PcapGlobalHeader maj mn tz sf sl net) =
  putWord32le 0xa1b2c3d4
    <> putWord16le maj
    <> putWord16le mn
    <> putInt32le tz
    <> putWord32le sf
    <> putWord32le sl
    <> putWord32le net

putPacketHeader :: PacketHeader -> Put
putPacketHeader (PacketHeader s us incl orig) =
  putWord32le s <> putWord32le us <> putWord32le incl <> putWord32le orig

putPacketRecord :: PacketRecord -> Put
putPacketRecord (PacketRecord hdr frame) =
  putPacketHeader hdr <> putEthFrame frame

putMacAddr :: MacAddr -> Put
putMacAddr (MacAddr hi lo) = putWord16be hi <> putWord32be lo

putEthFrame :: EthFrame -> Put
putEthFrame (EthFrame dst src et payload) =
  putMacAddr dst <> putMacAddr src <> putWord16be et <> putNetworkPayload payload

putNetworkPayload :: NetworkPayload -> Put
putNetworkPayload (NetIpv4 hdr opts trans) =
  putIpv4Header hdr <> putByteString opts <> putTransportPayload trans
putNetworkPayload (NetIpv6 hdr trans) =
  putIpv6Header hdr <> putTransportPayload trans
putNetworkPayload (NetOther bs) =
  putByteString bs

putIpv4Header :: Ipv4Header -> Put
putIpv4Header (Ipv4Header vi de tl idn ff ttl p cs src dst) =
  putWord8 vi
    <> putWord8 de
    <> putWord16be tl
    <> putWord16be idn
    <> putWord16be ff
    <> putWord8 ttl
    <> putWord8 p
    <> putWord16be cs
    <> putWord32be src
    <> putWord32be dst

putIpv6Header :: Ipv6Header -> Put
putIpv6Header (Ipv6Header vtf pl nh hl sh sl dh dl) =
  putWord32be vtf
    <> putWord16be pl
    <> putWord8 nh
    <> putWord8 hl
    <> putWord64be sh
    <> putWord64be sl
    <> putWord64be dh
    <> putWord64be dl

putTransportPayload :: TransportPayload -> Put
putTransportPayload (TransTcp hdr opts payload) =
  putTcpHeader hdr <> putByteString opts <> putByteString payload
putTransportPayload (TransUdp hdr (Just (DnsMessage dh db)) _) =
  putUdpHeader hdr <> putDnsHeader dh <> putByteString db
putTransportPayload (TransUdp hdr Nothing payload) =
  putUdpHeader hdr <> putByteString payload
putTransportPayload (TransOther bs) =
  putByteString bs

putTcpHeader :: TcpHeader -> Put
putTcpHeader (TcpHeader sp dp sq ak ofl w cs up) =
  putWord16be sp
    <> putWord16be dp
    <> putWord32be sq
    <> putWord32be ak
    <> putWord16be ofl
    <> putWord16be w
    <> putWord16be cs
    <> putWord16be up

putUdpHeader :: UdpHeader -> Put
putUdpHeader (UdpHeader sp dp ln cs) =
  putWord16be sp <> putWord16be dp <> putWord16be ln <> putWord16be cs

putDnsHeader :: DnsHeader -> Put
putDnsHeader (DnsHeader tx fl qd an ns ar) =
  putWord16be tx
    <> putWord16be fl
    <> putWord16be qd
    <> putWord16be an
    <> putWord16be ns
    <> putWord16be ar
